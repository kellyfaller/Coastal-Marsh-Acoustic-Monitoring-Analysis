# BirdNET combined-table year builder
# Author: Kelly Faller
#
# For each year, walks data/processed/birdNET/<year>/<ARU station>/ and finds every
# file whose name contains BirdNET_CombinedTable (searched recursively inside each station
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

source(here("R", "birdnet_io.R"))

cfg <- yaml::read_yaml(here("configs", "project.yml"))

# Years to process - years without a folder under the BirdNET raw directory yet are skipped
years <- cfg$params$birdnet_years
birdnet_dir <- here(cfg$paths$birdnet_raw_dir)

combine_year <- function(year) {
  year_dir <- file.path(birdnet_dir, year)
  # Years still being processed in BirdNET may not have a folder yet - skip them
  if (!dir.exists(year_dir)) {
    message("Year ", year, ": no folder yet at ", year_dir, " - skipping")
    return(NULL)
  }

  out <- read_station_folders(year_dir, label = paste("Year", year))
  if (is.null(out)) {
    warning("No detections found for ", year, " - skipping")
    return(NULL)
  }
  out[, Year := as.integer(year)]

  setcolorder(out, intersect(c("Year", "Site", "Station", "Date", "Hour", "Detection_Time",
                               "Common_Name", "Scientific_Name", "Confidence", "Start_s", "End_s",
                               "Audio_File", "Recording_Start", "File", "Source_Table"), names(out)))
  out
}

for (yr in years) {
  combined <- combine_year(yr)
  if (is.null(combined)) next
  out_path <- file.path(birdnet_dir, paste0("birdNET_", yr, "_combined.csv"))
  # Times are written without a "Z" because they are recorder clock time, not true UTC
  fwrite(combined, out_path, dateTimeAs = "write.csv")
  message("Wrote ", format(nrow(combined), big.mark = ","), " detections to ", out_path)
}
