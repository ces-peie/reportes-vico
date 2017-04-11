#------------------------------------------------------------------------------*
# Standard input functions
#------------------------------------------------------------------------------*
# Functions to help standardize input across reports from each site type and
# syndrome
#------------------------------------------------------------------------------*




#------------------------------------------------------------------------------*
# Query server for data ----
#------------------------------------------------------------------------------*
# Function query data from server given a view, a set of variables and a date
#------------------------------------------------------------------------------*

query_vico <- function(data_base, view, variables, max_date){
  # Get specified data
  .data <- dbGetQuery(
    conn = data_base,
    statement = paste(
      "SELECT",
      paste(variables, collapse = ", "),
      "FROM", view,
      "WHERE PDAInsertDate <= '", max_date, "';"
    )
  )
}


# End of script
