library(DBI)
library(RPostgres)
library(dplyr)
library(readr)
library(stringr)
library(glue)
library(lubridate)

con <- list(
  RPostgres::Postgres(),
  host = "aws-0-us-west-2.pooler.supabase.com", dbname = "postgres",
  user = "postgres.nfyvxsccktlojkvvnvdb", password = "spartinapatens", port =  5432, 
  sslmode  = "require")

# Root of your Kaleidoscope outputs folder
# Structure: kaleidoscope_root / year / station / acousticindex.csv
kaleidoscope_root <- "C:\\Users\\kelly\\Documents\\GitHub\\Soundscape-Analysis-Rare-Bird-Detection\\data\\processed\\kaleidoscope_outputs\\"

# Find all acousticindex.csv files recursively
all_index_files <- list.files(
  path       = kaleidoscope_root,
  pattern    = "acousticindex\\.csv$",
  full.names = TRUE,
  recursive  = TRUE
)

cat("Found", length(all_index_files), "acousticindex.csv files\n")
print(all_index_files)

# Read and annotate each file
all_indices <- lapply(all_index_files, function(filepath) {
  
  # Parse year and station from folder path
  # e.g. .../Kaleidoscope Outputs/2024/CAT_A1/acousticindex.csv
  parts   <- str_split(dirname(filepath), "[/\\\\]")[[1]]
  station <- tail(parts, 1)          # last folder = station
  year    <- tail(parts, 2)[1]       # second-to-last = year
  
  # Read the CSV
  df <- tryCatch(
    read_csv(filepath, show_col_types = FALSE),
    error = function(e) { cat("Error reading:", filepath, "\n"); return(NULL) }
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # Standardize column names to lowercase
  names(df) <- tolower(names(df))
  
  # Add provenance columns from folder path
  df <- df %>%
    mutate(
      station         = station,
      year            = as.integer(year),
      # Parse datetime from filename
      # filename format: Cattus_CAT_A1_Unit 1_20240629_050000.wav
      filename        = `in file`,
      date_chr        = str_extract(filename, "\\d{8}(?=_\\d{6})"),
      time_chr        = str_extract(filename, "(?<=_\\d{8}_)\\d{6}"),
      recorded_at     = ymd_hms(paste(date_chr, time_chr), tz = "UTC"),
      kaleidoscope_version = "5.9.0",
      source_file     = filepath
    ) %>%
    select(
      filename,
      station,
      year,
      recorded_at,
      date    = date,
      time    = time,
      hour,
      duration,
      ndsi,
      aci,
      adi,
      bi,
      kaleidoscope_version,
      source_file
    )
  
  cat(glue("  {station} {year}: {nrow(df)} recordings\n"))
  return(df)
})

# Combine all stations and years
combined <- bind_rows(Filter(Negate(is.null), all_indices))
cat(glue("\nTotal rows to import: {nrow(combined)}\n"))

# Write to database — append only, skip duplicates
# The unique key is filename + station to prevent double-importing
# Reconnect fresh before writing
con <- dbConnect(
  RPostgres::Postgres(),
  host     = "aws-0-us-west-2.pooler.supabase.com",
  dbname   = "postgres",
  user     = "postgres.nfyvxsccktlojkvvnvdb",
  password = "spartinapatens",
  port     = 5432,
  sslmode  = "require"
)

dbWriteTable(con, "acoustic_indices", combined, append = TRUE)

cat(glue("\nImported {nrow(combined)} acoustic index records\n"))

# Verify
summary <- dbGetQuery(con, "
  SELECT station, year,
         COUNT(*) as recordings,
         ROUND(AVG(aci)::numeric, 2) as avg_aci,
         ROUND(AVG(ndsi)::numeric, 4) as avg_ndsi
  FROM acoustic_indices
  GROUP BY station, year
  ORDER BY year, station
")
print(summary)

dbDisconnect(con)
