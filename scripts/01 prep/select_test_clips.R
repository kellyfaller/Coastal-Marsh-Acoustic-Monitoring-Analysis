# Test clip selection (analysis plan B1.3)
# Author: Kelly Faller
#
# Picks the clips to hand-label for each test set from stock BirdNET (M0) scores
# on the test audio. For each focal species at each site:
#   - 1 clip scoring 0 to 0.1
#   - 6 clips spread across 0.1 to 1.0 (one per equal-width bin, topped up at
#     random if a bin is empty)
#   - the highest- and lowest-scoring clips
#
# Input:  data/processed/model_eval/scores/M0/<test set>/<station>/BirdNET_CombinedTable.csv
#         Run BirdNET on the test audio with --min_conf 0.01 so low scores are
#         kept; otherwise the 0-0.1 bin is empty.
# Output: data/processed/model_eval/test_clips.csv - the clip list
#         data/processed/model_eval/test_labels.csv - the same clips with an empty
#         Labels column to fill in while listening in Raven. Type every sound
#         in the clip as codes separated by ";", e.g. "SALS;WIND". Leave
#         it blank (or "NONE") if nothing is there and "UNKNOWN" to drop the clip.

library(data.table)
library(dplyr)
library(yaml)
library(here)

source(here("R", "birdnet_io.R"))

cfg <- yaml::read_yaml(here("configs", "project.yml"))
ev <- cfg$evaluation
eval_dir <- here(cfg$paths$model_eval_dir)
focal <- unlist(ev$focal_species)
bins <- ev$test_clip_bins
set.seed(42)

select_for_group <- function(d) {
  low <- d %>% filter(Confidence < bins$low_max) %>% slice_sample(n = bins$n_low)
  high_pool <- d %>%
    filter(Confidence >= bins$low_max) %>%
    mutate(Bin = cut(Confidence, seq(bins$low_max, 1, length.out = bins$n_high + 1), include.lowest = TRUE))
  high <- high_pool %>% group_by(Bin) %>% slice_sample(n = 1) %>% ungroup()
  short <- bins$n_high - nrow(high)
  if (short > 0) {
    high <- bind_rows(high, high_pool %>% anti_join(high, by = "Clip_ID") %>% slice_sample(n = short))
  }
  extremes <- bind_rows(
    slice_max(d, Confidence, n = 1, with_ties = FALSE) %>% mutate(Stratum = "highest"),
    slice_min(d, Confidence, n = 1, with_ties = FALSE) %>% mutate(Stratum = "lowest")
  )
  bind_rows(
    mutate(low, Stratum = paste0("0-", bins$low_max)),
    mutate(high, Stratum = paste0(bins$low_max, "-1")),
    extremes
  ) %>%
    select(-any_of("Bin")) %>%
    distinct(Clip_ID, .keep_all = TRUE) %>%
    mutate(N_Low_Available = sum(d$Confidence < bins$low_max),
           N_High_Available = sum(d$Confidence >= bins$low_max))
}

clips <- lapply(unlist(ev$test_sets), function(test_set) {
  root <- file.path(eval_dir, "scores", ev$reference_model, test_set)
  if (!dir.exists(root)) {
    message("No M0 scores yet for test set '", test_set, "' (", root, ") - skipping")
    return(NULL)
  }
  m0 <- read_station_folders(root, label = paste("Test set", test_set))
  if (is.null(m0)) return(NULL)

  m0 <- as_tibble(m0) %>%
    mutate(Site = coalesce(Site, Station), Confidence = as.numeric(Confidence)) %>%
    inner_join(tibble(Code = names(focal), Common_Name = unname(focal)), by = "Common_Name") %>%
    mutate(Clip_ID = paste0(Audio_File, "@", sprintf("%.1f", as.numeric(Start_s))))

  if (!any(m0$Confidence < bins$low_max)) {
    warning("Test set '", test_set, "' has no scores below ", bins$low_max,
            ". Re-run BirdNET on this audio with --min_conf 0.01 to fill that bin.")
  }

  m0 %>%
    group_by(Code, Site) %>%
    group_modify(~ select_for_group(.x)) %>%
    ungroup() %>%
    mutate(Test_Set = test_set)
}) %>%
  bind_rows()

if (nrow(clips) == 0) stop("No M0 scores found under ", file.path(eval_dir, "scores", ev$reference_model))

# A clip picked for more than one species is labeled once
test_clips <- clips %>%
  group_by(Test_Set, Clip_ID) %>%
  summarise(
    Site = first(Site), Station = first(Station), Date = first(Date),
    Audio_File = first(Audio_File), File = first(File),
    Start_s = first(Start_s), End_s = first(End_s),
    Selected_For = paste(unique(paste0(Code, ":", Stratum)), collapse = ";"),
    M0_Scores = paste(unique(paste0(Code, "=", round(Confidence, 3))), collapse = ";"),
    .groups = "drop"
  ) %>%
  arrange(Test_Set, Site, Station, Date, Audio_File, Start_s)

fwrite(test_clips, file.path(eval_dir, "test_clips.csv"))
message("Wrote ", nrow(test_clips), " test clips to ", file.path(eval_dir, "test_clips.csv"))

# Never overwrite labels already typed in
labels_path <- file.path(eval_dir, "test_labels.csv")
if (file.exists(labels_path)) {
  message(labels_path, " already exists - not overwritten")
} else {
  # M0 scores are left out on purpose so they don't bias what you hear
  fwrite(test_clips %>% select(Test_Set, Site, Station, Date, Audio_File, File, Start_s, End_s) %>%
           mutate(Labels = "", Labeler = "", Notes = ""),
         labels_path)
  message("Wrote label sheet to ", labels_path)
}

print(clips %>% count(Test_Set, Code, Site, name = "Clips_Selected"))
