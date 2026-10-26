# SALSNET model evaluation helpers (analysis plan steps B1.5, B2, B5.4-B5.5, B6, B7)
# Author: Kelly Faller
#
# Every model (M0 stock BirdNET, M1-M5 custom classifiers) is scored the same way:
#   1. Hand-labeled test clips (one row per 3-s clip, Labels = "SALS;Wind" etc.)
#   2. The model's BirdNET output on the test audio
#   -> a score table with one row per Model x Test_Set x Clip x species,
#      holding the model's score and whether the species was really there.
# All metrics below are computed from that score table.

library(data.table)
library(dplyr)
library(tidyr)

# ---- Clip keys and labels -------------------------------------------------

# One ID per 3-s BirdNET segment: audio file name + segment start
clip_id <- function(audio_file, start_s) {
  paste0(basename(gsub("\\\\", "/", audio_file)), "@", sprintf("%.1f", as.numeric(start_s)))
}

# Labels are typed as one semicolon-separated cell per clip, e.g. "SALS;Wind".
# Empty or "None" = nothing there; any "Unknown" drops the clip (plan B1.4).
read_test_labels <- function(path) {
  labels <- fread(path, encoding = "UTF-8") %>%
    as_tibble() %>%
    mutate(
      Labels = toupper(trimws(coalesce(as.character(Labels), ""))),
      Clip_ID = clip_id(Audio_File, Start_s)
    )
  unknown <- grepl("(^|;)\\s*UNKNOWN\\s*(;|$)", labels$Labels)
  if (any(unknown)) message("Dropping ", sum(unknown), " clips labeled Unknown")
  labels[!unknown, ]
}

labels_long <- function(labels) {
  labels %>%
    select(Test_Set, Clip_ID, Labels) %>%
    separate_rows(Labels, sep = ";") %>%
    mutate(Label = trimws(Labels)) %>%
    filter(Label != "", Label != "NONE") %>%
    distinct(Test_Set, Clip_ID, Label)
}

# Convert Raven selection tables to clip labels. Each selection must give the
# audio file name and begin/end times (s) within that file. A clip gets a label
# when a selection overlaps it by at least min_overlap seconds.
raven_to_clip_labels <- function(selections, clips, file_col = "Begin File",
                                 begin_col = "File Offset (s)", end_col = NULL,
                                 label_col = "Species", min_overlap = 0.5) {
  sel <- as.data.table(selections)
  sel[, `:=`(
    Audio_File = basename(gsub("\\\\", "/", get(file_col))),
    Sel_Begin = as.numeric(get(begin_col)),
    Label = toupper(trimws(get(label_col)))
  )]
  sel[, Sel_End := if (is.null(end_col)) Sel_Begin + as.numeric(`Delta Time (s)`) else as.numeric(get(end_col))]

  cl <- as.data.table(clips)[, .(Test_Set, Audio_File = basename(gsub("\\\\", "/", Audio_File)),
                                 Start_s = as.numeric(Start_s), End_s = as.numeric(End_s))]
  hits <- merge(cl, sel[, .(Audio_File, Sel_Begin, Sel_End, Label)], by = "Audio_File",
                allow.cartesian = TRUE)
  hits <- hits[pmin(End_s, Sel_End) - pmax(Start_s, Sel_Begin) >= min_overlap]
  lab <- hits[, .(Labels = paste(sort(unique(Label)), collapse = ";")), by = .(Test_Set, Audio_File, Start_s)]
  out <- merge(cl, lab, by = c("Test_Set", "Audio_File", "Start_s"), all.x = TRUE)
  out[is.na(Labels), Labels := "NONE"]
  as_tibble(out)
}

# ---- Score table -----------------------------------------------------------

# Run folders are named <Model> for the full model, or <Model>_n<clips>_d<draw>
# for the clip-number runs in B5.4 (e.g. M1_n016_d03).
parse_run_name <- function(run) {
  m <- regmatches(run, regexec("^(M[0-9]+)(?:_n([0-9]+)_d([0-9]+))?$", run, perl = TRUE))
  tibble(
    Run_ID = run,
    Model = vapply(m, function(x) if (length(x)) x[2] else NA_character_, ""),
    Clips_Per_Class = vapply(m, function(x) if (length(x) && x[3] != "") as.integer(x[3]) else NA_integer_, 1L),
    Draw = vapply(m, function(x) if (length(x) && x[4] != "") as.integer(x[4]) else NA_integer_, 1L)
  )
}

# scores: standardized BirdNET output with Run_ID and Test_Set columns.
# labels: output of read_test_labels(). focal: named vector Code -> Common_Name.
# Clips the model gave no score for a species get 0 (below BirdNET's min_conf),
# so run every model on the test audio with a low min_conf (0.01).
build_score_table <- function(scores, labels, focal) {
  sc <- as_tibble(scores) %>%
    mutate(Clip_ID = clip_id(File, Start_s), Confidence = as.numeric(Confidence)) %>%
    inner_join(tibble(Code = names(focal), Common_Name = unname(focal)), by = "Common_Name") %>%
    group_by(Run_ID, Test_Set, Clip_ID, Code) %>%
    summarise(Score = max(Confidence), .groups = "drop")

  truth <- labels_long(labels) %>%
    filter(Label %in% names(focal)) %>%
    transmute(Test_Set, Clip_ID, Code = Label, Truth = 1L)

  grid <- labels %>%
    select(Test_Set, Clip_ID, any_of(c("Site", "Station", "Date"))) %>%
    distinct(Test_Set, Clip_ID, .keep_all = TRUE) %>%
    inner_join(distinct(sc, Run_ID, Test_Set), by = "Test_Set", relationship = "many-to-many") %>%
    crossing(Code = names(focal))

  missing <- anti_join(distinct(sc, Test_Set, Clip_ID), grid, by = c("Test_Set", "Clip_ID"))
  if (nrow(missing) > 0) {
    message(nrow(missing), " scored segments are not in the labeled test clips and are ignored")
  }

  grid %>%
    left_join(sc, by = c("Run_ID", "Test_Set", "Clip_ID", "Code")) %>%
    left_join(truth, by = c("Test_Set", "Clip_ID", "Code")) %>%
    mutate(Score = coalesce(Score, 0), Truth = coalesce(Truth, 0L)) %>%
    left_join(parse_run_name(unique(.$Run_ID)), by = "Run_ID")
}

# Station-day level (plan "How results are judged"): a station-day is positive if
# any labeled clip from it holds the species; its score is the highest clip score.
station_day_scores <- function(score_table) {
  score_table %>%
    group_by(across(any_of(c("Run_ID", "Model", "Clips_Per_Class", "Draw"))),
             Test_Set, Station, Date, Code) %>%
    summarise(Score = max(Score), Truth = max(Truth), .groups = "drop") %>%
    mutate(Clip_ID = paste(Station, Date, sep = "_"))
}

# ---- Metrics ---------------------------------------------------------------

# Precision/recall at every distinct score (ties handled as one cut-off)
pr_curve <- function(score, truth) {
  o <- order(score, decreasing = TRUE)
  s <- score[o]; t <- truth[o]
  last <- c(diff(s) != 0, TRUE)
  tp <- cumsum(t)[last]; fp <- cumsum(1 - t)[last]
  tibble(Threshold = s[last], TP = tp, FP = fp,
         Precision = tp / (tp + fp), Recall = tp / sum(truth))
}

# Average precision (PR-AUC), step-wise as in scikit-learn
average_precision <- function(score, truth) {
  if (sum(truth) == 0) return(NA_real_)
  pr <- pr_curve(score, truth)
  sum(diff(c(0, pr$Recall)) * pr$Precision)
}

# ROC-AUC: chance a random positive clip outscores a random negative one
roc_auc <- function(score, truth) {
  n_pos <- sum(truth); n_neg <- sum(1 - truth)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  (sum(rank(score)[truth == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

threshold_metrics <- function(score, truth, threshold) {
  pred <- score >= threshold
  tp <- sum(pred & truth == 1); fp <- sum(pred & truth == 0); fn <- sum(!pred & truth == 1)
  precision <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  recall <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  tibble(Threshold = threshold, TP = tp, FP = fp, FN = fn, TN = sum(!pred & truth == 0),
         Precision = precision, Recall = recall,
         F1 = if (isTRUE(precision + recall > 0)) 2 * precision * recall / (precision + recall) else NA_real_)
}

# Lowest cut-off whose precision reaches the target, and the recall left at it
precision_cutoff <- function(score, truth, target = 0.9) {
  pr <- pr_curve(score, truth) %>% filter(Precision >= target)
  if (nrow(pr) == 0) return(tibble(Cutoff_Empirical = NA_real_, Recall_At_Cutoff = NA_real_))
  best <- pr[which.min(pr$Threshold), ]
  tibble(Cutoff_Empirical = best$Threshold, Recall_At_Cutoff = best$Recall)
}

# Wood & Kahl (2024): logistic regression of correct ~ logit(score) on the
# model's detections, solved for the score where P(correct) = target (B6.4).
# Valid even though test clips were picked by score bin, because it models
# precision conditional on score.
logistic_cutoff <- function(score, truth, target = 0.9) {
  keep <- score > 0
  d <- data.frame(correct = truth[keep], logit_score = qlogis(pmin(pmax(score[keep], 1e-4), 1 - 1e-4)))
  if (nrow(d) < 5 || length(unique(d$correct)) < 2) return(list(cutoff = NA_real_, model = NULL))
  fit <- suppressWarnings(glm(correct ~ logit_score, family = binomial, data = d))
  b <- coef(fit)
  cutoff <- if (is.na(b[2]) || b[2] <= 0) NA_real_ else plogis((qlogis(target) - b[1]) / b[2])
  # A cut-off above every observed score means the target is never reached
  if (isTRUE(cutoff > max(score[keep]))) cutoff <- NA_real_
  list(cutoff = unname(cutoff), model = fit)
}

# All metrics per run x test set x species. Species with min_positives or fewer
# positive clips are flagged as too rare to score reliably (B1.5).
evaluate_scores <- function(score_table, threshold = 0.5, target_precision = 0.9, min_positives = 5) {
  score_table %>%
    group_by(across(any_of(c("Run_ID", "Model", "Clips_Per_Class", "Draw"))), Test_Set, Code) %>%
    group_modify(function(d, key) {
      bind_cols(
        tibble(N_Clips = nrow(d), N_Pos = sum(d$Truth), N_Neg = sum(1 - d$Truth),
               AP = average_precision(d$Score, d$Truth), ROC_AUC = roc_auc(d$Score, d$Truth)),
        threshold_metrics(d$Score, d$Truth, threshold),
        precision_cutoff(d$Score, d$Truth, target_precision),
        tibble(Cutoff_Logistic = logistic_cutoff(d$Score, d$Truth, target_precision)$cutoff)
      )
    }) %>%
    ungroup() %>%
    mutate(Too_Rare = N_Pos <= min_positives)
}

# Which sparrow is mistaken for which (focal species only). Predicted = the
# highest-scoring focal species at or above the threshold; "None" otherwise.
focal_confusion <- function(score_table, threshold = 0.5) {
  by_clip <- score_table %>%
    group_by(Run_ID, Test_Set, Clip_ID) %>%
    summarise(
      Actual = {
        present <- Code[Truth == 1]
        if (length(present) == 0) "None" else if (length(present) > 1) "Multiple" else present
      },
      Predicted = if (max(Score) >= threshold) Code[which.max(Score)] else "None",
      .groups = "drop"
    )
  by_clip %>% count(Run_ID, Test_Set, Actual, Predicted, name = "Clips")
}

# Error list (B2.2-B2.3): for each false positive / false negative, what was
# actually labeled in the clip (other sparrows, wind, boat, insects...).
error_audit <- function(score_table, labels, threshold = 0.5) {
  what_was_there <- labels_long(labels) %>%
    group_by(Test_Set, Clip_ID) %>%
    summarise(In_Clip = paste(sort(Label), collapse = ";"), .groups = "drop")

  score_table %>%
    mutate(Error = case_when(Score >= threshold & Truth == 0 ~ "False positive",
                             Score < threshold & Truth == 1 ~ "False negative")) %>%
    filter(!is.na(Error)) %>%
    left_join(what_was_there, by = c("Test_Set", "Clip_ID")) %>%
    mutate(In_Clip = coalesce(In_Clip, "NONE"))
}

# Count how often each labeled sound shows up in a model's errors. Sounds that
# recur become candidate extra training classes (B2.3).
error_sources <- function(errors, focal_codes) {
  errors %>%
    separate_rows(In_Clip, sep = ";") %>%
    filter(!(Error == "False negative" & In_Clip == Code)) %>%
    count(Run_ID, Test_Set, Code, Error, Sound = In_Clip, name = "Clips", sort = TRUE) %>%
    mutate(Sound_Type = if_else(Sound %in% focal_codes, "Focal sparrow", "Other sound"))
}

# ---- Comparing models (B6) -------------------------------------------------

# Paired bootstrap of the AP difference between a model and the reference
# model (M0): clips are resampled once and both models are scored on the same
# resample, so the CI reflects the difference, not each model's own noise.
bootstrap_ap_diff <- function(score_table, run, ref = "M0", n_boot = 1000, seed = 42) {
  wide <- score_table %>%
    filter(Run_ID %in% c(run, ref)) %>%
    select(Run_ID, Test_Set, Code, Clip_ID, Score, Truth) %>%
    pivot_wider(names_from = Run_ID, values_from = Score) %>%
    filter(!is.na(.data[[run]]), !is.na(.data[[ref]]))
  if (nrow(wide) == 0) return(NULL)

  set.seed(seed)
  wide %>%
    group_by(Test_Set, Code) %>%
    group_modify(function(d, key) {
      obs <- average_precision(d[[run]], d$Truth) - average_precision(d[[ref]], d$Truth)
      diffs <- replicate(n_boot, {
        i <- sample.int(nrow(d), replace = TRUE)
        average_precision(d[[run]][i], d$Truth[i]) - average_precision(d[[ref]][i], d$Truth[i])
      })
      tibble(AP_Diff = obs,
             CI_Low = quantile(diffs, 0.025, na.rm = TRUE),
             CI_High = quantile(diffs, 0.975, na.rm = TRUE),
             Prob_Better = mean(diffs > 0, na.rm = TRUE))
    }) %>%
    ungroup() %>%
    mutate(Run_ID = run, Reference = ref, .before = 1)
}

# B6.1-B6.3: rank by primary-species AP on the independent test set, check the
# model also beats the reference on the local set, and among models within
# tie_tolerance AP of the best, prefer the one with fewer training sources.
rank_models <- function(metrics, comparisons, registry, species = "SALS",
                        independent = "independent", local = "local",
                        ref = "M0", tie_tolerance = 0.02) {
  n_sources <- tibble(Model = names(registry),
                      N_Sources = vapply(registry, function(m) length(m$sources), 1L))
  full <- metrics %>% filter(is.na(Clips_Per_Class), Code == species)

  ranked <- full %>%
    select(Model, Test_Set, AP) %>%
    pivot_wider(names_from = Test_Set, values_from = AP, names_prefix = "AP_") %>%
    left_join(comparisons %>% filter(Code == species, Test_Set == local) %>%
                transmute(Model = Run_ID, Local_Diff_vs_Ref = AP_Diff, Local_CI_Low = CI_Low),
              by = "Model") %>%
    left_join(n_sources, by = "Model")

  ind_col <- paste0("AP_", independent)
  if (!ind_col %in% names(ranked)) {
    warning("No scores yet on the '", independent, "' test set; ranking on '", local, "' instead")
    ind_col <- paste0("AP_", local)
  }
  best_ap <- max(ranked[[ind_col]][ranked$Model != ref], na.rm = TRUE)

  ranked %>%
    mutate(
      Rank_AP = rank(-.data[[ind_col]], ties.method = "min"),
      Beats_Ref_Local = Model == ref | coalesce(Local_CI_Low > 0, FALSE),
      Within_Tie = Model != ref & .data[[ind_col]] >= best_ap - tie_tolerance
    ) %>%
    arrange(Rank_AP) %>%
    mutate(Chosen = {
      pool <- which(Within_Tie & Beats_Ref_Local)
      chosen <- if (length(pool)) pool[order(N_Sources[pool], -.data[[ind_col]][pool])][1] else NA
      seq_along(Model) == chosen & !is.na(chosen)
    })
}

# ---- Clip-number curves and iteration log (B5.4-B5.5) ------------------------

clip_curve_summary <- function(metrics, species = "SALS") {
  metrics %>%
    filter(!is.na(Clips_Per_Class), Code == species) %>%
    group_by(Model, Test_Set, Clips_Per_Class) %>%
    summarise(Draws = n(), AP_Mean = mean(AP, na.rm = TRUE),
              AP_Low = quantile(AP, 0.025, na.rm = TRUE),
              AP_High = quantile(AP, 0.975, na.rm = TRUE), .groups = "drop")
}

# Adds this round's results to the iteration log. Runs already in the log are
# replaced; rows added by hand (e.g. failed runs, Status = "failed") are kept.
update_iteration_log <- function(metrics, registry, path) {
  new <- metrics %>%
    mutate(Evaluated = as.character(Sys.Date()), Status = "ok",
           Sources = vapply(Model, function(m) paste(registry[[m]]$sources, collapse = "+"), "")) %>%
    select(Run_ID, Model, Sources, Clips_Per_Class, Draw, Test_Set, Code, N_Pos, N_Neg,
           AP, ROC_AUC, Threshold, Precision, Recall, F1, Cutoff_Logistic, Evaluated, Status)
  old <- if (file.exists(path)) as_tibble(fread(path)) else NULL
  if (!is.null(old)) {
    old <- old %>% filter(!(Run_ID %in% new$Run_ID & Status == "ok")) %>%
      mutate(across(everything(), as.character))
    new <- bind_rows(old, mutate(new, across(everything(), as.character)))
  }
  fwrite(new, path)
  invisible(new)
}

# ---- Station-day agreement on all audio (B7) ---------------------------------

# detections: model output on all ARU audio (Station, Date, Code, Score).
# verified:   listened station-days (Station, Date, Code, Present = 0/1).
# A station-day counts as model-present with min_detections or more
# detections at or above the threshold (B7.2).
station_day_agreement <- function(detections, verified, threshold, min_detections = 2) {
  model_days <- detections %>%
    filter(Score >= threshold) %>%
    count(Station, Date, Code, name = "Detections") %>%
    mutate(Model_Present = as.integer(Detections >= min_detections))

  verified %>%
    left_join(model_days, by = c("Station", "Date", "Code")) %>%
    mutate(Model_Present = coalesce(Model_Present, 0L), Detections = coalesce(Detections, 0L)) %>%
    group_by(Code) %>%
    summarise(
      Days = n(),
      Both = sum(Model_Present == 1 & Present == 1),
      Model_Only = sum(Model_Present == 1 & Present == 0),
      Verified_Only = sum(Model_Present == 0 & Present == 1),
      Neither = sum(Model_Present == 0 & Present == 0),
      .groups = "drop"
    ) %>%
    mutate(
      Precision = Both / (Both + Model_Only),
      Recall = Both / (Both + Verified_Only),
      Agreement = (Both + Neither) / Days,
      # Cohen's kappa: agreement beyond what chance alone would give
      Chance = ((Both + Model_Only) * (Both + Verified_Only) +
                  (Verified_Only + Neither) * (Model_Only + Neither)) / Days^2,
      Kappa = (Agreement - Chance) / (1 - Chance)
    ) %>%
    select(-Chance)
}
