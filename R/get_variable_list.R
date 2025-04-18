# European Commission
# ARDECO database
# R packege "ardeco" exposing ARDECO data to be used in R
#
# Function: get_variable_list
# Input: none
# Output: list of variables
#         - code: variable code
#         - description: variable description
#         - datasets: list of datasets defined into the variable
#                     - unit: unit of measure for the dataset
#                     - sector: sector of the dataset
#
# Description: return the list of the variable collected into ARDECO exposing
#              code, description and the set of datasets defined for that variable.
#              for each dataset is returned the unit of measure and the sector.
#              For variable with just only one sector, this usually is identified
#              by "Total" code.
#
#' @import dplyr
#' @import ghql
#' @import httr
#' @import jsonlite
#' @import tidyr
#' @import stringr
#' @export

ardeco_get_variable_list <- function() {
  # set code variable to NULL (for CRAN check)
  code <- NULL

  # root of the URL to access to graphQL API for ARDECO
  link <- 'https://urban.jrc.ec.europa.eu/ardeco-api-v2/graphql'
  conn <- GraphqlClient$new(url=link)

  # build the graphql query to recover the list of variables
  # and related description
  #query <- 'query{variableList{code, description, nutsLevel}}'
  query <- 'query{variableList(export: true){code, description}}'
  new <- Query$new()$query('link', query)


  tryCatch({
    # submit the GraphQL API request
    result <- conn$exec(new$link) %>% fromJSON(flatten = F)
  }, error = function(e) {
    # strErr <- paste("Error call: ", link)
    # stop(strErr)
  })

  if (is.null(result)) {
    print("Error during execution. Check API avalaibility")
    return(NULL)
  }

  # convert the result in formatted list
  variable_list <- result$data$variableList %>% as_tibble()


  # return the formatted data
  return(variable_list)
}
