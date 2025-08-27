fp <- function(...) normalizePath(file.path(...), mustWork = FALSE)

load_config <- function(path = "configs/project.yml") {
  yml <- yaml::read_yaml(path)
  if (is.null(yml$paths) || is.null(yml$outputs) || is.null(yml$params))
    cli::cli_abort("Config file missing required top-level keys: paths/outputs/params.")
  yml
}
