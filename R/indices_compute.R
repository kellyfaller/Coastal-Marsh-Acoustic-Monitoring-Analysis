# pure helpers that do not touch global state
attach_filename_metadata <- function(df) {
  if (is.null(df) || !nrow(df)) return(df)
  df |>
    dplyr::mutate(
      DB_THRESHOLD = as.character(dplyr::coalesce(.data$DB_THRESHOLD, NA_character_)),
      site_name = stringr::str_extract(.data$FILENAME, "CAT_\\d+|LHC_\\d+|[A-Za-z0-9_-]+"),
      ts_raw = stringr::str_extract(.data$FILENAME, "[0-9]{8}_[0-9]{6}"),
      ts_fmt = dplyr::if_else(!is.na(.data$ts_raw), stringr::str_replace(.data$ts_raw, "_", " "), NA_character_),
      recording_datetime = suppressWarnings(lubridate::ymd_hms(.data$ts_fmt, tz = "UTC"))
    )
}

indices_bind_and_clean <- function(indices_raw) {
  if (is.null(indices_raw) || !nrow(indices_raw)) return(tibble::tibble())
  indices_raw |>
    attach_filename_metadata() |>
    dplyr::filter(!is.na(recording_datetime))
}

