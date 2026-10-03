# Shared functions for the thesis analysis skeleton (configs/thesis_framework.yml).
# Sourced by scripts/01 prep/Thesis Data Prep.Rmd and the chapter notebooks in scripts/03 thesis chapters/.
# Nothing in here is used by 01-Point_Counts.Rmd.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
})

# ---- Config ---------------------------------------------------------------------------------------

# The config file can be swapped (e.g. a personal copy with local paths) by setting the THESIS_CONFIG environment variable.
load_thesis_config <- function(path = Sys.getenv("THESIS_CONFIG", here::here("configs", "thesis_framework.yml"))) {
  cfg <- yaml::read_yaml(path)
  resolve <- function(p) {
    if (is.null(p)) return(p)
    vapply(p, function(x) if (fs::is_absolute_path(x)) x else here::here(x), character(1), USE.NAMES = FALSE)
  }
  cfg$paths   <- lapply(cfg$paths, resolve)
  cfg$outputs <- lapply(cfg$outputs, resolve)
  invisible(lapply(cfg$outputs, dir.create, recursive = TRUE, showWarnings = FALSE))
  # point count file paths come from the point count notebook's config
  cfg$pc <- yaml::read_yaml(cfg$paths$point_count_config)
  cfg$pc$paths <- lapply(cfg$pc$paths, resolve)
  cfg
}

# TRUE if a file (or any file matching a glob) exists
has_file <- function(paths) length(unlist(lapply(paths, Sys.glob))) > 0

# reads every csv as text so files that disagree on column types can be stacked; convert columns afterwards
read_csv_text <- function(path) readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), progress = FALSE)

# ---- Species names and 4-letter alpha codes -------------------------------------------------------

# Automatic 4-letter banding code from a common name (1 word: first 4 letters; 2 words: 2+2; 3 words: 1+1+2;
# 4+ words: first letter of each of the first 4). Hyphens split words. A few species break the rule
# (e.g. Saltmarsh Sparrow = SALS), so known exceptions go in alpha_code_overrides in the YAML.
alpha_code <- function(common_name, overrides = list()) {
  vapply(common_name, function(nm) {
    if (is.na(nm)) return(NA_character_)
    if (!is.null(overrides[[nm]])) return(overrides[[nm]])
    w <- strsplit(toupper(gsub("[^A-Za-z -]", "", nm)), "[ -]+")[[1]]
    w <- w[w != ""]
    if (!length(w)) return(NA_character_)
    switch(as.character(min(length(w), 4)),
           "1" = substr(w[1], 1, 4),
           "2" = paste0(substr(w[1], 1, 2), substr(w[2], 1, 2)),
           "3" = paste0(substr(w[1], 1, 1), substr(w[2], 1, 1), substr(w[3], 1, 2)),
           "4" = paste0(substr(w[1], 1, 1), substr(w[2], 1, 1), substr(w[3], 1, 1), substr(w[4], 1, 1)))
  }, character(1), USE.NAMES = FALSE)
}

# genus + species only (Arbimon sometimes stores subspecies, e.g. "Nycticorax nycticorax hoactli"), with older names
# swapped for current ones (scientific_synonyms in the YAML, e.g. Ammodramus nelsoni -> Ammospiza nelsoni)
sci_binomial <- function(x, synonyms = NULL) {
  out <- stringr::word(stringr::str_squish(x), 1, 2)
  if (length(synonyms)) {
    syn <- unlist(synonyms)
    out <- ifelse(out %in% names(syn), unname(syn[out]), out)
  }
  out
}

# one row per species: scientific name, common name, alpha code, guild. Extra names (e.g. from BirdNET) are added.
build_species_crosswalk <- function(cfg, extra = NULL) {
  base <- read_csv_text(cfg$paths$species_list) %>%
    dplyr::transmute(scientific_name = sci_binomial(scientific_name, cfg$params$scientific_synonyms), common_name, guild)
  if (!is.null(extra)) {
    base <- dplyr::bind_rows(base, extra %>% dplyr::transmute(scientific_name = sci_binomial(scientific_name, cfg$params$scientific_synonyms), common_name) %>%
                               dplyr::filter(!scientific_name %in% base$scientific_name))
  }
  base %>%
    dplyr::distinct(scientific_name, .keep_all = TRUE) %>%
    dplyr::mutate(alpha_code = alpha_code(common_name, cfg$params$alpha_code_overrides))
}

# ---- Point counts ---------------------------------------------------------------------------------

# Long-format point counts for every year in project.yml (any path named <site>_<year>_pc). Same fixes as the
# point count notebook: both date formats, year from the file name, numeric counts.
read_point_counts_thesis <- function(cfg) {
  tz <- cfg$params$timezone
  pc_paths <- cfg$pc$paths[grepl("_\\d{4}_pc$", names(cfg$pc$paths))]
  pc <- purrr::imap_dfr(pc_paths, function(p, nm) {
    read_csv_text(p) %>% dplyr::mutate(Year = as.integer(stringr::str_extract(nm, "\\d{4}")))
  }) %>%
    dplyr::rename(Point_ID = `Point ID`) %>%
    dplyr::mutate(
      SurveyDate = dplyr::if_else(grepl("/", SurveyDate), lubridate::mdy(SurveyDate, quiet = TRUE), lubridate::dmy(SurveyDate, quiet = TRUE)),
      Datetime   = lubridate::parse_date_time(paste(SurveyDate, SurveyTime), orders = c("Ymd IMp", "Ymd HMS", "Ymd HM"), tz = tz),
      TotalCount = suppressWarnings(as.numeric(TotalCount)),
      VisitNum   = as.integer(VisitNum))
  pts <- read_csv_text(cfg$pc$paths$site_metadata) %>%
    dplyr::transmute(Point_ID, Lat = as.numeric(Lat), Long = as.numeric(Long))
  dplyr::left_join(pc, pts, by = "Point_ID")
}

# ---- Arbimon backup (relational tables) -----------------------------------------------------------

# Reads every table in an unzipped Arbimon backup. Multi-part tables (recordings.0001 ... recordings.0011)
# are stacked. Returns a named list: recordings, sites, species, pattern_matchings, pattern_matching_rois, ...
read_arbimon_backup <- function(dir) {
  files <- list.files(dir, pattern = "\\.\\d{4}\\.csv$", full.names = TRUE)
  if (!length(files)) {
    warning("No Arbimon backup tables found in ", dir)
    return(list())
  }
  family <- sub("\\.\\d{4}\\.csv$", "", basename(files))
  lapply(split(files, family), function(f) dplyr::bind_rows(lapply(sort(f), read_csv_text)))
}

# picks the first column name that exists
first_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit)) hit[1] else NA_character_
}

# Turns the raw backup tables into tidy, joined tables. Arbimon `datetime` is the local time on the
# recorder (it matches the wav file name); `datetime_utc` is UTC.
arbimon_tables <- function(bk, cfg) {
  tz <- cfg$params$timezone
  out <- list()

  out$recordings <- bk$recordings %>%
    dplyr::transmute(
      recording_id, site_id,
      datetime_local = lubridate::parse_date_time(datetime, c("mdy HMS", "Ymd HMS"), tz = tz),
      datetime_utc   = lubridate::parse_date_time(datetime_utc, c("mdy HMS", "Ymd HMS"), tz = "UTC"),
      duration_s     = as.numeric(duration),
      sample_rate    = as.numeric(sample_rate),
      source_file    = stringr::str_match(meta, '"filename":"([^"]+)"')[, 2])   # the url column (signed S3 links) is dropped on purpose

  if (!is.null(bk$sites)) {
    s <- bk$sites
    out$sites <- tibble::tibble(
      site_id   = s$site_id,
      station   = s[[first_col(s, c("name", "site_name"))]],
      site_lat  = suppressWarnings(as.numeric(s[[first_col(s, c("lat", "latitude"))]])),
      site_long = suppressWarnings(as.numeric(s[[first_col(s, c("lon", "long", "longitude"))]])))
  } else {
    warning("No sites table in the backup; station names will be the Arbimon site_id")
    out$sites <- tibble::tibble(site_id = unique(out$recordings$site_id), station = site_id, site_lat = NA_real_, site_long = NA_real_)
  }

  out$species <- bk$species %>%
    dplyr::mutate(scientific_name = sci_binomial(scientific_name, cfg$params$scientific_synonyms), family = stringr::str_squish(family)) %>%
    dplyr::select(species_id, songtype_id, scientific_name, family, songtype)

  rec_site <- out$recordings %>% dplyr::left_join(out$sites, by = "site_id")

  if (!is.null(bk$pattern_matching_rois)) {
    # pattern matching jobs: name, template used, playlist searched and the score threshold (stored as JSON in `parameters`)
    pm_names <- if (!is.null(bk$pattern_matchings)) {
      pmj <- bk$pattern_matchings
      tibble::tibble(pattern_matching_id = pmj$pattern_matching_id,
                     pm_name = pmj[[first_col(pmj, c("name", "pattern_matching_name"))]],
                     template_id = if ("template_id" %in% names(pmj)) pmj$template_id else NA_character_,
                     template_name = if ("template_name" %in% names(pmj)) pmj$template_name else NA_character_,
                     playlist_name = if ("playlist_name" %in% names(pmj)) pmj$playlist_name else NA_character_,
                     pm_threshold = if ("parameters" %in% names(pmj))
                       suppressWarnings(as.numeric(stringr::str_match(pmj$parameters, '"threshold":\\s*([0-9.]+)')[, 2])) else NA_real_)
    } else tibble::tibble(pattern_matching_id = character(), pm_name = character(), template_id = character(),
                          template_name = character(), playlist_name = character(), pm_threshold = numeric())

    zero <- cfg$params$arbimon_pm_zero_means
    out$pm_rois <- bk$pattern_matching_rois %>%
      dplyr::mutate(dplyr::across(c(x1, x2, y1, y2, score), as.numeric),
                    validated = suppressWarnings(as.integer(validated))) %>%
      dplyr::left_join(pm_names, by = "pattern_matching_id") %>%
      dplyr::left_join(rec_site, by = "recording_id") %>%
      dplyr::left_join(out$species, by = c("species_id", "songtype_id")) %>%
      dplyr::mutate(
        detection_time = datetime_local + x1,
        status = dplyr::case_when(validated == 1 ~ "present",
                                  validated == 0 & zero == "absent" ~ "absent",
                                  TRUE ~ "unreviewed"))
  }

  if (!is.null(bk$recording_validations)) {
    v <- bk$recording_validations
    present_cols <- grep("^present", names(v), value = TRUE)
    v$present <- if (length(present_cols)) {
      pc <- as.data.frame(lapply(v[present_cols], function(x) suppressWarnings(as.integer(x))))
      ifelse(rowSums(!is.na(pc)) == 0, NA_integer_, as.integer(rowSums(pc == 1, na.rm = TRUE) > 0))
    } else NA_integer_
    out$validations <- v %>%
      dplyr::select(-dplyr::any_of(c("scientific_name", "songtype", "family"))) %>%   # these come from the species table instead
      dplyr::left_join(rec_site, by = "recording_id") %>%
      dplyr::left_join(out$species, by = c("species_id", "songtype_id"))
  }

  if (!is.null(bk$rfm_classifications)) {
    # random forest model results: one row per recording per model job. Arbimon exports a 0/1 `present` call
    # (no probability), so the random forest is evaluated at a single operating point.
    rf <- bk$rfm_classifications %>% dplyr::select(-dplyr::any_of(c("scientific_name", "songtype")))
    score_col <- first_col(rf, c("score", "max_vector_value", "probability", "present"))
    rf$rfm_score <- suppressWarnings(as.numeric(rf[[score_col]]))
    out$rfm <- rf
    if ("recording_id" %in% names(rf)) out$rfm <- dplyr::left_join(out$rfm, rec_site, by = "recording_id")
    if (all(c("species_id", "songtype_id") %in% names(rf))) out$rfm <- dplyr::left_join(out$rfm, out$species, by = c("species_id", "songtype_id"))
  }

  # small lookup tables are passed through as-is
  for (tbl in c("templates", "playlists", "playlist_recordings", "pattern_matchings")) out[[tbl]] <- bk[[tbl]]
  if (!is.null(out$templates)) out$templates <- dplyr::select(out$templates, -dplyr::any_of("url"))   # signed S3 links
  out
}

# ---- BirdNET --------------------------------------------------------------------------------------

# Station, unit and file start time from a recorder file name such as
# "D:\...\Cattus_CAT_A1_Unit 1_20240713_230000.wav" -> station CAT_A1, unit "Unit 1", 2024-07-13 23:00:00
parse_recorder_filename <- function(path, tz) {
  b <- sub("\\.(wav|flac|mp3)$", "", basename(gsub("\\\\", "/", path)), ignore.case = TRUE)
  tok <- strsplit(b, "_")
  purrr::map_dfr(tok, function(t) {
    n <- length(t)
    u <- grep("^Unit ?\\d+$", t)
    tibble::tibble(
      station    = if (length(u) && u[1] >= 3) paste(t[u[1] - 2], t[u[1] - 1], sep = "_") else NA_character_,
      unit       = if (length(u)) gsub(" ", "", t[u[1]]) else NA_character_,
      file_start = lubridate::ymd_hms(paste(t[n - 1], t[n]), tz = tz, quiet = TRUE))
  })
}

# Reads one or more BirdNET-Analyzer combined tables (glob patterns allowed) into a standard format.
# One row per 3-second detection with its real date-time, station and the 1-minute window it falls in
# (Arbimon splits recordings into 1-minute files, so minutes are the common unit for comparisons).
read_birdnet_tables <- function(paths, cfg, source = "BirdNET") {
  files <- unlist(lapply(paths, Sys.glob))
  if (!length(files)) return(NULL)
  bn <- dplyr::bind_rows(lapply(files, read_csv_text))
  names(bn) <- gsub("_+$", "", tolower(gsub("[^A-Za-z]+", "_", names(bn))))
  bn <- bn %>%
    dplyr::transmute(start_s = as.numeric(start_s), end_s = as.numeric(end_s),
                     scientific_name = ifelse(scientific_name == common_name, scientific_name,
                                              sci_binomial(scientific_name, cfg$params$scientific_synonyms)),
                     common_name, confidence = as.numeric(confidence), file) %>%
    dplyr::filter(!is.na(start_s), confidence >= cfg$params$birdnet_min_confidence)
  meta <- parse_recorder_filename(unique(bn$file), cfg$params$timezone) %>% dplyr::mutate(file = unique(bn$file))
  bn %>%
    dplyr::left_join(meta, by = "file") %>%
    dplyr::mutate(
      source = source,
      detection_time = file_start + start_s,
      minute = lubridate::floor_date(detection_time, "minute"),
      Year = lubridate::year(detection_time),
      is_bird = !(common_name %in% cfg$params$non_bird_labels) & scientific_name != common_name &
                !(stringr::word(common_name, -1) %in% cfg$params$non_bird_last_words))
}

# presence threshold for each species: species-specific value from the YAML, otherwise the default
birdnet_threshold <- function(scientific_name, cfg) {
  th <- unlist(cfg$params$birdnet_species_thresholds)
  out <- unname(th[scientific_name])
  ifelse(is.na(out), cfg$params$birdnet_default_threshold, out)
}

# ---- Space and time helpers -----------------------------------------------------------------------

haversine_m <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad; dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  6371000 * 2 * atan2(sqrt(a), sqrt(1 - a))
}

# ARU deployments (station x year) paired with the nearest point count point; flags pairs farther apart than allowed
match_aru_to_points <- function(cfg, pc) {
  aru <- read_csv_text(cfg$paths$aru_metadata)
  names(aru) <- gsub("^\ufeff", "", names(aru))
  aru <- aru %>%
    dplyr::transmute(Year = as.integer(year), Site = stringr::str_squish(gsub("_", " ", site)), station,
                     unit, date_deployed = lubridate::mdy(date_deployed),
                     aru_lat = as.numeric(lat), aru_long = as.numeric(long))
  pts <- pc %>% dplyr::distinct(Point_ID, Lat, Long) %>% dplyr::filter(!is.na(Lat))
  # distance from every ARU deployment (rows) to every point count point (columns)
  d <- outer(seq_len(nrow(aru)), seq_len(nrow(pts)),
             function(i, j) haversine_m(aru$aru_lat[i], aru$aru_long[i], pts$Lat[j], pts$Long[j]))
  nearest <- apply(d, 1, which.min)
  aru %>%
    dplyr::mutate(nearest_point = pts$Point_ID[nearest],
                  distance_m = round(d[cbind(seq_len(nrow(aru)), nearest)]),
                  paired = distance_m <= cfg$params$pc_match_distance_m)
}

in_season <- function(date, cfg) {
  md <- format(as.Date(date), "%m-%d")
  md >= cfg$params$season_start & md <= cfg$params$season_end
}

# Dawn / Day / Dusk / Night for each time, from sunrise and sunset at the study area (suncalc if installed,
# otherwise fixed clock hours). Dawn = 1 h before to 3 h after sunrise; Dusk = 2 h before to 1 h after sunset.
diel_period <- function(time, cfg) {
  tz <- cfg$params$timezone
  d <- as.Date(time, tz = tz)
  if (requireNamespace("suncalc", quietly = TRUE)) {
    sun <- suncalc::getSunlightTimes(date = unique(d), lat = cfg$params$site_lat, lon = cfg$params$site_long,
                                     keep = c("sunrise", "sunset"), tz = tz)
    idx <- match(d, sun$date)
    sr <- sun$sunrise[idx]; ss <- sun$sunset[idx]
  } else {
    sr <- as.POSIXct(paste(d, "05:45:00"), tz = tz); ss <- as.POSIXct(paste(d, "20:15:00"), tz = tz)
  }
  dplyr::case_when(
    time >= sr - 3600 & time < sr + 3 * 3600 ~ "Dawn",
    time >= ss - 2 * 3600 & time < ss + 3600 ~ "Dusk",
    time >= sr + 3 * 3600 & time < ss - 2 * 3600 ~ "Day",
    TRUE ~ "Night")
}

# ---- Species accumulation and sampling sufficiency (incidence-based, Chao et al. 2014) -----------

# presence: one row per sample x species detected; samples: every sample surveyed (so empty samples count)
incidence_from <- function(presence, samples) {
  Y <- presence %>% dplyr::distinct(sample_id, species) %>% dplyr::count(species, name = "Y") %>% dplyr::pull(Y)
  list(Y = Y, T = dplyr::n_distinct(samples))
}

sufficiency_stats <- function(inc, targets = c(0.9, 0.95)) {
  Y <- inc$Y; T <- inc$T
  Sobs <- length(Y); Q1 <- sum(Y == 1); Q2 <- sum(Y == 2); U <- sum(Y)
  Q0 <- if (Q2 > 0) ((T - 1) / T) * Q1^2 / (2 * Q2) else ((T - 1) / T) * Q1 * (Q1 - 1) / 2
  chao2 <- Sobs + Q0
  coverage <- if (U == 0) NA_real_ else if (Q2 > 0) 1 - (Q1 / U) * ((T - 1) * Q1 / ((T - 1) * Q1 + 2 * Q2)) else
    1 - (Q1 / U) * ((T - 1) * (Q1 - 1) / ((T - 1) * (Q1 - 1) + 2))
  needed <- sapply(targets, function(g) {
    if (Q0 <= 0 || g * chao2 <= Sobs) return(0)
    ceiling(log(1 - (g * chao2 - Sobs) / Q0) / log(1 - Q1 / (T * Q0 + Q1)))
  })
  out <- data.frame(Samples = T, Observed = Sobs, Uniques = Q1, Duplicates = Q2, Chao2 = chao2,
                    Completeness = Sobs / chao2, Coverage = coverage)
  for (i in seq_along(targets)) out[[paste0("Samples_to_", targets[i] * 100, "pct")]] <- needed[i]
  out
}

accumulation_curve <- function(inc, max_t = 2 * inc$T) {
  Y <- inc$Y; T <- inc$T
  Sobs <- length(Y); Q1 <- sum(Y == 1); Q2 <- sum(Y == 2)
  Q0 <- if (Q2 > 0) ((T - 1) / T) * Q1^2 / (2 * Q2) else ((T - 1) / T) * Q1 * (Q1 - 1) / 2
  t <- seq_len(max_t)
  S <- sapply(t, function(tt) {
    if (tt <= T) sum(1 - exp(lchoose(T - Y, tt) - lchoose(T, tt)))
    else if (Q0 == 0) Sobs
    else Sobs + Q0 * (1 - (1 - Q1 / (T * Q0 + Q1))^(tt - T))
  })
  data.frame(Samples = t, Species = S, Part = ifelse(t <= T, "Observed (rarefied)", "Extrapolated"))
}

# ---- Detection histories for occupancy -------------------------------------------------------------

# detections: one row per site x occasion where the species was detected; surveyed: every site x occasion
# surveyed. Returns a site x occasion 0/1 matrix (NA = not surveyed), rows in the order of `sites`.
detection_matrix <- function(detections, surveyed, sites) {
  surveyed %>%
    dplyr::left_join(detections %>% dplyr::distinct(site, occasion) %>% dplyr::mutate(det = 1L), by = c("site", "occasion")) %>%
    dplyr::mutate(det = tidyr::replace_na(det, 0L)) %>%
    tidyr::pivot_wider(names_from = occasion, values_from = det, names_sort = TRUE) %>%
    dplyr::right_join(tibble::tibble(site = sites), by = "site") %>%
    dplyr::arrange(match(site, sites)) %>%
    dplyr::select(-site) %>%
    as.matrix()
}

# ---- Classifier evaluation (Ch2 / Ch3) -------------------------------------------------------------

# precision / recall / F1 at each threshold. truth = 1/0, score = classifier score (NA = not detected -> 0)
classifier_metrics <- function(truth, score, thresholds = seq(0.05, 0.95, by = 0.05)) {
  score <- tidyr::replace_na(score, 0)
  purrr::map_dfr(thresholds, function(th) {
    pred <- score >= th
    tp <- sum(pred & truth == 1); fp <- sum(pred & truth == 0); fn <- sum(!pred & truth == 1); tn <- sum(!pred & truth == 0)
    precision <- ifelse(tp + fp == 0, NA_real_, tp / (tp + fp))
    recall <- ifelse(tp + fn == 0, NA_real_, tp / (tp + fn))
    tibble::tibble(threshold = th, tp, fp, fn, tn, precision, recall,
                   f1 = ifelse(is.na(precision) | is.na(recall) | precision + recall == 0, NA_real_,
                               2 * precision * recall / (precision + recall)))
  })
}

# area under the precision-recall curve (average precision)
average_precision <- function(truth, score) {
  score <- tidyr::replace_na(score, 0)
  o <- order(score, decreasing = TRUE)
  t <- truth[o]
  if (sum(t == 1) == 0) return(NA_real_)
  tp <- cumsum(t == 1)
  sum((tp / seq_along(t))[t == 1]) / sum(t == 1)
}

# ---- Small output helpers --------------------------------------------------------------------------

save_thesis_fig <- function(plot, name, cfg, height = 7, width = 12) {
  ggplot2::ggsave(file.path(cfg$outputs$figures_dir, name), plot, height = height, width = width, dpi = 300, bg = "white")
  plot
}

kbl_simple <- function(df, caption = NULL, digits = 2) {
  knitr::kable(df, caption = caption, digits = digits) %>%
    kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
}

# writes a "TO DO" box into the knitted report when an input isn't there yet
todo_note <- function(...) {
  cat("\n> **To do:** ", ..., "\n\n", sep = "")
}
