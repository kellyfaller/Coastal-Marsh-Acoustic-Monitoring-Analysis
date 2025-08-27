read_aru_locations <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE)
  assert_cols(df, c("site_name","longitude","latitude"), "ARU locations")
}

read_arbimon <- function(pm_path, meta_path) {
  pm   <- vroom::vroom(pm_path, show_col_types = FALSE)
  meta <- vroom::vroom(meta_path, show_col_types = FALSE)
  list(pm = pm, meta = meta)
}

read_indices_folder <- function(dir) {
  # read whatever exists, tag with index name, bind safely
  safe_read <- \(p) if (file.exists(p)) readr::read_csv(p, show_col_types=FALSE) else NULL
  lst <- list(
    ACI  = safe_read(file.path(dir,"resultACI_F.csv")),
    ADI  = safe_read(file.path(dir,"resultADI_F_35db.csv")),
    AEI  = safe_read(file.path(dir,"resultAEI_F.csv")),
    BI   = safe_read(file.path(dir,"resultBI_F.csv")),
    H    = safe_read(file.path(dir,"resultH_F.csv")),
    NDSI = safe_read(file.path(dir,"resultNDSI_F.csv"))
  )
  purrr::list_rbind(purrr::imap(lst, \(x, nm) if (!is.null(x)) dplyr::mutate(x, index = nm)))
}

write_parquet <- function(df, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(df, path)
  path
}

