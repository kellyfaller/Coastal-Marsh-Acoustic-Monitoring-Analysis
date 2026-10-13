# Reading SHARP point count datasheets. Used by scripts/02 analysis/01-Point_Counts.Rmd and R/thesis_functions.R
# so every point count file is cleaned the same way, whoever entered it.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
})

# Site names and point IDs written different ways on different datasheets
site_names <- c("LHC" = "Lighthouse Center", "LIGHTHOUSE CENTER" = "Lighthouse Center", "LIGHTHOUSE_CENTER" = "Lighthouse Center",
                "CAT" = "Cattus Island", "CATTUS" = "Cattus Island", "CATTUS ISLAND" = "Cattus Island", "CATTUS_ISLAND" = "Cattus Island")

standardize_site <- function(x) {
  key <- toupper(stringr::str_squish(x))
  out <- unname(site_names[key])
  ifelse(is.na(out), stringr::str_squish(x), out)
}

# "LHC01", "lhc_1", "LHC-01" -> "LHC_01"
standardize_point <- function(x) {
  m <- stringr::str_match(toupper(stringr::str_squish(x)), "^([A-Z]+)[_ -]?0*(\\d+)$")
  ifelse(is.na(m[, 1]), x, sprintf("%s_%02d", m[, 2], as.integer(m[, 3])))
}

# Survey dates come in several formats depending on who entered them:
#   2025 sheets: 30-06-2025 (day-month-year)   2026 sheets: 6/28/2026 (month/day/year)   ISO: 2023-06-15
parse_survey_date <- function(x) {
  x <- stringr::str_squish(x)
  dplyr::case_when(
    grepl("^\\d{4}-\\d{1,2}-\\d{1,2}", x) ~ lubridate::ymd(x, quiet = TRUE),
    grepl("/", x)                         ~ lubridate::mdy(x, quiet = TRUE),
    grepl("^\\d{1,2}-\\d{1,2}-\\d{4}", x) ~ lubridate::dmy(x, quiet = TRUE),
    TRUE                                  ~ as.Date(NA))
}

# Reads one point count csv. Every column is read as text (files disagree on column types) and "." is treated as
# missing. `year`: the survey year when the file holds one season; NULL when a file covers several years (the year
# is then taken from SurveyDate in the formatting step). `code_synonyms` / `observer_aliases`: named lists
# (old = new) so the same species or person is written the same way in every file.
recode_with <- function(x, map) {
  if (!length(map)) return(x)
  map <- unlist(map)
  ifelse(x %in% names(map), unname(map[x]), x)
}

read_point_count_file <- function(path, year = NULL, code_synonyms = NULL, observer_aliases = NULL) {
  df <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), na = c("", "NA", "."),
                        progress = FALSE)
  names(df) <- stringr::str_squish(gsub("^﻿", "", names(df)))      # Excel byte-order mark / stray spaces
  df %>%
    dplyr::rename(Point_ID = `Point ID`) %>%
    dplyr::mutate(
      Point_ID = standardize_point(Point_ID),
      Site     = standardize_site(Site),
      Tide     = stringr::str_to_title(stringr::str_squish(Tide)),          # "high/rising" -> "High/Rising"
      DistBand = tolower(stringr::str_squish(DistBand)),                    # ">100M" -> ">100m"
      AlphaCode = recode_with(stringr::str_squish(AlphaCode), code_synonyms),
      Observer = recode_with(toupper(stringr::str_squish(Observer)), observer_aliases),
      Year     = if (is.null(year)) NA_real_ else year,
      Source_File = basename(path))
}

# Year for files that cover more than one season (Year is NA after reading)
fill_year_from_date <- function(df) {
  df %>% dplyr::mutate(Year = dplyr::coalesce(Year, as.numeric(lubridate::year(parse_survey_date(SurveyDate)))))
}

# Which point count files are listed in a config's paths: anything named <...>_pc. A 4-digit year in the name
# (e.g. lhc_2025_pc) is the survey year; names with two years or none (lhc_2023_2024_pc) take the year from the dates.
point_count_files <- function(paths) {
  pc <- paths[grepl("_pc$", names(paths))]
  yrs <- stringr::str_extract_all(names(pc), "\\d{4}")
  tibble::tibble(key = names(pc), path = unlist(pc),
                 year = vapply(yrs, function(y) if (length(y) == 1) as.numeric(y) else NA_real_, numeric(1)))
}

read_all_point_counts <- function(paths, skip_missing = TRUE, code_synonyms = NULL, observer_aliases = NULL) {
  files <- point_count_files(paths)
  missing <- files$path[!file.exists(files$path)]
  if (length(missing)) {
    if (!skip_missing) stop("Point count file(s) not found: ", paste(missing, collapse = ", "))
    warning("Skipping point count file(s) not found: ", paste(basename(missing), collapse = ", "))
    files <- files[file.exists(files$path), ]
  }
  dplyr::bind_rows(lapply(seq_len(nrow(files)), function(i)
    read_point_count_file(files$path[i], if (is.na(files$year[i])) NULL else files$year[i],
                          code_synonyms = code_synonyms, observer_aliases = observer_aliases))) %>%
    fill_year_from_date()
}
