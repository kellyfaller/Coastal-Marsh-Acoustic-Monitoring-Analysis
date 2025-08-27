read_birdnet_predictions <- function(path) {
  df <- vroom::vroom(path, show_col_types = FALSE)
  assert_cols(df, c("site","dt_start","wav_path","segment_start_s","species","logit","confidence"), "BirdNET predictions")
  df
}

pr_curve <- function(scores, labels) {
  yardstick::pr_auc_vec(factor(labels, levels=c(0,1)), scores)
}

