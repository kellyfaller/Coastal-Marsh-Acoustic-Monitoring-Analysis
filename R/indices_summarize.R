indices_daily_site_summary <- function(df) {
  assert_cols(df, c("recording_datetime","site_name","index","VALUE"), "indices")
  df |>
    dplyr::mutate(day = lubridate::floor_date(recording_datetime, "day")) |>
    dplyr::group_by(day, site_name, index) |>
    dplyr::summarise(mean = mean(VALUE, na.rm=TRUE), se = stats::sd(VALUE, na.rm=TRUE)/sqrt(dplyr::n()), .groups="drop")
}

