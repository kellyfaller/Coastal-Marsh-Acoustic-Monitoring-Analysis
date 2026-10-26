# Shared helpers for reading BirdNET output tables
# Author: Kelly Faller
#
# Used by scripts/01 prep/birdnet_combine_year.R and the model evaluation scripts.
# Expected folder layout:  <root>/<Station>/.../BirdNET_CombinedTable.csv

library(data.table)

site_from_station <- function(station) {
  fcase(
    startsWith(toupper(station), "CAT"), "Cattus_Island",
    startsWith(toupper(station), "LHC"), "Lighthouse_Center",
    default = NA_character_
  )
}

# BirdNET writes slightly different headers depending on version/output format
# (CSV: "Start (s)", "Common name"; Raven: "Begin Time (s)", "Begin Path").
# Clean the names and map them onto one standard set.
standardize_birdnet <- function(dt) {
  clean <- tolower(gsub("[^A-Za-z0-9]+", "_", names(dt)))
  clean <- gsub("^_|_$", "", clean)
  lookup <- c(
    start_s = "Start_s", begin_time_s = "Start_s",
    end_s = "End_s", end_time_s = "End_s",
    scientific_name = "Scientific_Name", species_code = "Species_Code",
    common_name = "Common_Name", confidence = "Confidence",
    file = "File", begin_path = "File", begin_file = "File"
  )
  new <- ifelse(clean %in% names(lookup), lookup[clean], names(dt))
  setnames(dt, new)
  # Fall back to position for old headerless/unknown layouts
  if (!all(c("Start_s", "Common_Name", "Confidence") %in% names(dt)) && ncol(dt) >= 6) {
    setnames(dt, 1:6, c("Start_s", "End_s", "Scientific_Name", "Common_Name", "Confidence", "File"))
  }
  dt
}

read_combined_table <- function(path) {
  # Set the separator: file paths contain spaces, which fread can mistake for one
  sep <- if (grepl("\\.csv$", path, ignore.case = TRUE)) "," else "\t"
  dt <- tryCatch(fread(path, sep = sep, encoding = "UTF-8", fill = TRUE),
                 error = function(e) {
                   warning("Could not read ", path, ": ", conditionMessage(e))
                   NULL
                 })
  if (is.null(dt) || nrow(dt) == 0) return(NULL)
  standardize_birdnet(dt)
}

# Stack every BirdNET_CombinedTable under root/<Station>/ and add Station, Site,
# Audio_File and recording date/time (from ..._YYYYMMDD_HHMMSS.wav file names).
# Returns NULL when nothing is found.
read_station_folders <- function(root, label = basename(root)) {
  stations <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  message(label, ": ", length(stations), " station folders (", paste(stations, collapse = ", "), ")")

  per_station <- lapply(stations, function(station) {
    paths <- list.files(file.path(root, station),
                        pattern = "BirdNET_CombinedTable",  # also matches prefixed names like Unit4_CAT_05_2026_BirdNET_CombinedTable.csv
                        full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    if (length(paths) == 0) {
      warning("No BirdNET_CombinedTable found for ", station, " (", label, ")")
      return(NULL)
    }
    dt <- rbindlist(lapply(paths, function(p) {
      x <- read_combined_table(p)
      if (!is.null(x)) x[, Source_Table := substring(p, nchar(root) + 2)]
      x
    }), fill = TRUE)
    message("  ", station, ": ", length(paths), " table(s), ", format(nrow(dt), big.mark = ","), " detections")
    if (nrow(dt) == 0) return(NULL)
    dt[, Station := station]
  })

  out <- rbindlist(per_station, fill = TRUE)
  if (nrow(out) == 0) return(NULL)

  out[, Site := site_from_station(Station)]
  if (anyNA(out$Site)) {
    warning("Stations with no site assignment: ",
            paste(unique(out[is.na(Site), Station]), collapse = ", "))
  }

  # Recording timestamp from the audio file name (..._YYYYMMDD_HHMMSS.wav)
  out[, Audio_File := basename(gsub("\\\\", "/", as.character(File)))]
  stamp <- regmatches(out$Audio_File, regexpr("[0-9]{8}_[0-9]{6}", out$Audio_File))
  has_stamp <- grepl("[0-9]{8}_[0-9]{6}", out$Audio_File)
  out[, Recording_Start := as.POSIXct(NA_real_, tz = "UTC")]
  out[has_stamp, Recording_Start := as.POSIXct(stamp, format = "%Y%m%d_%H%M%S", tz = "UTC")]
  out[, `:=`(
    Date = as.IDate(Recording_Start),
    Detection_Time = Recording_Start + as.numeric(Start_s)
  )]
  out[, Hour := as.integer(format(Detection_Time, "%H", tz = "UTC"))]
  if (any(!has_stamp)) {
    warning(sum(!has_stamp), " detections have no YYYYMMDD_HHMMSS stamp in the file name; dates left NA")
  }
  out
}
