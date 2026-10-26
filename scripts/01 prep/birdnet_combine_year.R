# BirdNET combined-table year builder
# Author: Kelly Faller
#
# For each year, walks data/processed/birdNET/<year>/<ARU station>/ and finds every
# file named BirdNET_CombinedTable.* (searched recursively inside each station
# folder). All tables for that year are stacked into one file with added columns:
#   Station - the ARU station folder name (e.g. CAT_A1, LHC_01)
#   Site    - Cattus_Island (stations starting with CAT) or
#             Lighthouse_Center (stations starting with LHC)
# Recording date/time are parsed from the audio file name (..._YYYYMMDD_HHMMSS.wav).
#
# Output: data/processed/birdNET/birdNET_<year>_combined.csv

library(data.table)
library(yaml)
library(here)

cfg <- yaml::read_yaml(here("configs", "project.yml"))

# Years to process - each must have a folder under the BirdNET raw directory
years <- cfg$params$birdnet_years
birdnet_dir <- here(cfg$paths$birdnet_raw_dir)

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
  dt <- tryCatch(fread(path, encoding = "UTF-8", fill = TRUE),
                 error = function(e) {
                   warning("Could not read ", path, ": ", conditionMessage(e))
                   NULL
                 })
  if (is.null(dt) || nrow(dt) == 0) return(NULL)
  standardize_birdnet(dt)
}

combine_year <- function(year) {
  year_dir <- file.path(birdnet_dir, year)
  if (!dir.exists(year_dir)) stop("Year folder not found: ", year_dir)

  stations <- list.dirs(year_dir, full.names = FALSE, recursive = FALSE)
  message("Year ", year, ": ", length(stations), " station folders (",
          paste(stations, collapse = ", "), ")")

  per_station <- lapply(stations, function(station) {
    paths <- list.files(file.path(year_dir, station),
                        pattern = "^BirdNET_CombinedTable",
                        full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    if (length(paths) == 0) {
      warning("No BirdNET_CombinedTable found for ", station, " (", year, ")")
      return(NULL)
    }
    dt <- rbindlist(lapply(paths, function(p) {
      x <- read_combined_table(p)
      if (!is.null(x)) x[, Source_Table := substring(p, nchar(year_dir) + 2)]
      x
    }), fill = TRUE)
    message("  ", station, ": ", length(paths), " table(s), ", format(nrow(dt), big.mark = ","), " detections")
    if (nrow(dt) == 0) return(NULL)
    dt[, Station := station]
  })

  out <- rbindlist(per_station, fill = TRUE)
  if (nrow(out) == 0) stop("No detections found for ", year)

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
    Year = as.integer(year),
    Date = as.IDate(Recording_Start),
    Detection_Time = Recording_Start + as.numeric(Start_s)
  )]
  out[, Hour := as.integer(format(Detection_Time, "%H", tz = "UTC"))]
  if (any(!has_stamp)) {
    warning(sum(!has_stamp), " detections have no YYYYMMDD_HHMMSS stamp in the file name; dates left NA")
  }

  setcolorder(out, intersect(c("Year", "Site", "Station", "Date", "Hour", "Detection_Time",
                               "Common_Name", "Scientific_Name", "Confidence", "Start_s", "End_s",
                               "Audio_File", "Recording_Start", "File", "Source_Table"), names(out)))
  out
}

for (yr in years) {
  combined <- combine_year(yr)
  out_path <- file.path(birdnet_dir, paste0("birdNET_", yr, "_combined.csv"))
  # Times are written without a "Z" because they are recorder clock time, not true UTC
  fwrite(combined, out_path, dateTimeAs = "write.csv")
  message("Wrote ", format(nrow(combined), big.mark = ","), " detections to ", out_path)
}
