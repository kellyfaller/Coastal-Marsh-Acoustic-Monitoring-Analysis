# Reading Kaleidoscope Pro acoustic index outputs. Shared by
#   scripts/01 prep/Kaleidoscope Data Prep Script.Rmd   (writes the combined csv the analysis reads)
#   scripts/01 prep/kaleidoscope_data_prep_db.R         (uploads the same rows to the database)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
})

# Reads every acousticindex.csv under `root`. Expected folder layout: root/<year>/<station>/acousticindex.csv
# (station and year come from the folders because newer file names only have the unit, e.g. Unit4_20250603_210000.wav).
# Times are the local recorder time (file names and Kaleidoscope's DATE/TIME columns are local), so they are
# labelled with `tz` rather than UTC.
read_kaleidoscope_indices <- function(root, tz = "America/New_York", kaleidoscope_version = "5.9.0") {
  files <- list.files(root, pattern = "acousticindex\\.csv$", full.names = TRUE, recursive = TRUE)
  if (!length(files)) stop("No acousticindex.csv files found under ", root)

  out <- lapply(files, function(filepath) {
    parts   <- strsplit(normalizePath(dirname(filepath), winslash = "/", mustWork = FALSE), "/")[[1]]
    station <- tail(parts, 1)          # last folder = station
    year    <- tail(parts, 2)[1]       # second-to-last = year
    df <- tryCatch(readr::read_csv(filepath, show_col_types = FALSE, col_types = readr::cols(.default = "c")),
                   error = function(e) { message("Error reading: ", filepath); NULL })
    if (is.null(df) || nrow(df) == 0) return(NULL)
    names(df) <- tolower(names(df))

    df %>%
      dplyr::mutate(
        filename = `in file`,
        station  = station,
        year     = suppressWarnings(as.integer(year)),
        # DATE ("2024-06-29") + TIME ("05:00:00.000") from Kaleidoscope; file name date/time as a fallback
        recorded_at = lubridate::ymd_hms(paste(date, substr(time, 1, 8)), tz = tz, quiet = TRUE),
        recorded_at = dplyr::coalesce(recorded_at, lubridate::ymd_hms(
          paste(stringr::str_extract(filename, "\\d{8}(?=_\\d{6})"), stringr::str_extract(filename, "(?<=\\d{8}_)\\d{6}")),
          tz = tz, quiet = TRUE)),
        date = as.Date(recorded_at, tz = tz),
        time = format(recorded_at, "%H:%M:%S"),
        hour = as.integer(hour),
        dplyr::across(c(duration, ndsi, aci, adi, bi), as.numeric),
        kaleidoscope_version = kaleidoscope_version,
        source_file = filepath) %>%
      dplyr::select(filename, station, year, recorded_at, date, time, hour, duration, ndsi, aci, adi, bi,
                    kaleidoscope_version, source_file)
  })
  dplyr::bind_rows(Filter(Negate(is.null), out))
}

# Quick checks: rows per station/year, unparsed times, folder year vs recording year, duplicate recordings
check_kaleidoscope_indices <- function(df) {
  list(
    by_station = df %>% dplyr::group_by(station, year) %>%
      dplyr::summarise(recordings = dplyr::n(), first = min(recorded_at), last = max(recorded_at), .groups = "drop"),
    problems = tibble::tibble(
      `Unparsed date-times` = sum(is.na(df$recorded_at)),
      `Folder year differs from recording year` = sum(df$year != lubridate::year(df$recorded_at), na.rm = TRUE),
      `Duplicate station + file rows` = sum(duplicated(df[c("station", "filename")])),
      `Missing index values` = sum(!stats::complete.cases(df[c("ndsi", "aci", "adi", "bi")]))))
}
