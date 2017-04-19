#'-----------------------------------------------------------------------------*
#' Get community location data
#'-----------------------------------------------------------------------------*
#' Get community coding data from the VICo sql-server
#' All data should be loaded each time the reports are generated, and once a 
#' report has been completed the raw and processed datasets should be saved
#' as a snapshot
#'-----------------------------------------------------------------------------*


#'-----------------------------------------------------------------------------*
#' Prepare analysis environment
#'-----------------------------------------------------------------------------*

# Load used Packages
library(package = tidyverse) # tidy data

# Load geo data
load(file = "data/geo/geo.RData")

# Set snapshot file name
snapshot_file <- paste0("data/locations.RData")


# Get data from server
if(!file.exists(snapshot_file)){
  # Load package for data access
  library(package = DBI)
  
  # Connect to server
  data_base <- dbConnect(
    odbc::odbc(), "PEIEServer",
    uid = scan("data/user", what = "character"),
    pwd = scan("data/password", what = "character")
  )
  
  # Get data up to last Friday
  locations <- dbGetQuery(
    conn = data_base,
    statement = "select * from LegalValue.centros_poblados"
  )
  
  
  # Keep necessary data
  locations <- locations %>% 
    tbl_df %>%
    select(
      subject_department = cod_depto,
      subject_municipality = cod_depto_muni,
      subject_community = comunidad,
      community_code = cod_censo
    )
  
  
  # Save snapshot
  save(locations, file = snapshot_file)
} else {
  load(snapshot_file)
}
