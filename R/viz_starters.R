plot_index_timeseries <- function(summary_df) {
  ggplot2::ggplot(summary_df, ggplot2::aes(day, mean, color = index)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = mean - se, ymax = mean + se, fill = index), alpha = 0.15, color = NA) +
    ggplot2::labs(x=NULL, y="Index value", color=NULL, fill=NULL) +
    ggplot2::theme_minimal()
}

plot_confidence_hist <- function(preds) {
  ggplot2::ggplot(preds, ggplot2::aes(confidence, fill = species)) +
    ggplot2::geom_histogram(bins = 40, position = "identity", alpha = 0.3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "BirdNET confidence", y = "Segments")
}

