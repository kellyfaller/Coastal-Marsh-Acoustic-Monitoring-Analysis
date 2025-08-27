#!/usr/bin/env Rscript
future::plan(future::multisession, workers = load_config()$params$cores)
targets::tar_make()
