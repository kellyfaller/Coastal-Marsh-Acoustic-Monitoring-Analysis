library(targets)
library(tarchetypes)

tar_option_set(packages = c(
  "dplyr","readr","vroom","stringr","lubridate","arrow","yaml","cli","ggplot2"
))

config <- load_config()

list(
  tar_target(aru_locations, read_aru_locations(config$paths$aru_locations)),
  tar_target(indices_raw, read_indices_folder(config$paths$indices_dir)),
  tar_target(indices_clean, indices_bind_and_clean(indices_raw)),
  tar_target(indices_daily, indices_daily_site_summary(indices_clean)),
  tar_target(indices_daily_parquet,
             write_parquet(indices_daily, file.path(config$outputs$processed_dir, "indices_daily.parquet")),
             format = "file"),
  tar_target(birdnet_preds, read_birdnet_predictions(config$paths$birdnet_predictions), cue = tar_cue(mode="always")),
  tar_target(fig_indices, plot_index_timeseries(indices_daily), format = "qs"),
  tar_target(fig_conf, plot_confidence_hist(birdnet_preds), format = "qs")
)
