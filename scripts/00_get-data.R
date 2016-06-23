#'-----------------------------------------------------------------------------*
#' Get data for the reports
#'-----------------------------------------------------------------------------*
#' Get data used by the report form the VICo sql-server
#' All data should be loaded each time the reports are generated, and once a 
#' report has been completed the raw and processed datasets should be saved
#' as a snapshot
#'-----------------------------------------------------------------------------*


#'-----------------------------------------------------------------------------*
#' Prepare analysis environment
#'-----------------------------------------------------------------------------*

# Load used Packages
library(package = RODBC) # Connect to sql-server and download data
library(package = tidyr) # Clean up data
library(package = dplyr) # Manage data


# Define database to cache the data locally
vico_file <- "vico.sqlite"

data_base_path <- paste(
  base = "./data/cache",
  date = Sys.Date(),
  file = vico_file,
  sep = "_"
)


# Define sources for data groups
sources <- list(
  subject = "Clinicos.Basica_Respira",
  eligibility = "Clinicos.Basica_Respira",
  respiratory = "Clinicos.Basica_Respira",
  diarrheal = "Clinicos.Basica_Diarrea",
  febrile = "Clinicos.Basica_Febril"
)

# Define variables needed for the analysis.
# All tables with subject data should contain the SubjectID variable so they can
# be related
variables <- list(
  # General subject information
  subject = c(
    "SubjectID", "SASubjectID", "pacienteInscritoVico", "actualAdmitido",
    "PDAInsertDate", "fechaHoraAdmision", "epiYearAdmision", "epiWeekAdmision",
    "SiteName", "SiteType", "NombreDepartamento", "NombreMunicipio", "catchment",
    "edadAnios", "edadMeses", "edadDias", "fechaDeNacimiento"
  ),
  # Some additional information regarding eligibility
  eligibility = c(
    "SubjectID",
    "elegibleRespira", "elegibleDiarrea", "elegibleFebril"
  ),
  # Information specific to the respiratory syndrome
  respiratory = c(
    "SubjectID",
    "viralPCR_Hizo",
    "viralPCR_FluA",
    "viralPCR_FluB"
  ),
  # Information specific to the diarrheal syndrome
  diarrheal = c(
    "SubjectID",
    "pruebaRotavirusHizo",
    "rotavirus"
  ),
  # Information specific to the febrile syndrome
  febrile = c(
    "SubjectID",
    "dengue_ELISA_lgM",
    "dengue_ELISA_lgM_convaleciente",
    "dengue_ELISA_lgG",
    "dengue_PCR",
    "dengue_tipo1",
    "dengue_tipo2",
    "dengue_tipo3",
    "dengue_tipo4"
  )
)




#'-----------------------------------------------------------------------------*
#' Download data
#'-----------------------------------------------------------------------------*

if(!file.exists(data_base_path)){
  # Create cache database
  local_db <- src_sqlite(path = data_base_path, create = TRUE)
  
  
  # Connect to server
  data_base <- odbcDriverConnect(
    paste(
      "Driver=SQL Server",
      "Server=FSX-GT3",
      "Database=ViCo",
      "Trusted_Connection=Yes",
      sep =";"
    )
  )
  
  
  # Query the necessary data and store as tables in a sqlite database
  for(source_idx in seq_along(sources)){
    # Get source and destination names
    source_table <- sources[[source_idx]]
    source_group <- names(sources[source_idx])
    
    # Build query
    query <- paste0(
      "SELECT ", paste(variables[[source_group]], collapse = ", "), "\n\t",
      "FROM ", source_table, "\n\t"
    )
    
    # Get data from server
    message("Querying view: ", source_table)
    df <- sqlQuery(channel = data_base, query = query, stringsAsFactors = FALSE)
    
    # Save data to local sqlite database
    message("Creating table: ", source_group, "\n")
    copy_to(dest = local_db, df = df, name = source_group, temporary = FALSE)
  }
  
  # Disconnect from database
  odbcClose(data_base)
  
  # Clean up environment
  rm(data_base, source_idx, source_table, source_group, query, df)
  
} else {
  # Connect to local cache database
  local_db <- src_sqlite(path = data_base_path, create = FALSE)
}



# Clean up environment
rm(data_base_path, sources, variables, vico_file)



# End of script
