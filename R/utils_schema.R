assert_cols <- function(df, required, label = "data") {
  miss <- setdiff(required, names(df))
  if (length(miss)) cli::cli_abort("{label} is missing columns: {paste(miss, collapse=', ')}")
  invisible(df)
}

