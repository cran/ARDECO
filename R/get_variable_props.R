# European Commission
# ARDECO database
#
# Function: get_variable_props
# Input: variable=one of the variable code returned by ardeco_get_variable_list
# Output: the lowest nutslevel for which exist data for the input variable
# Description: recover the variable info (lowest nutlevel and description) using a graphql API
#
#' @import dplyr
#' @import ghql
#' @import httr
#' @import jsonlite
#' @import tidyr
#' @import stringr

ardeco_get_variable_props <- function(variable) {
  # set code variable to NULL (for CRAN check)
  code <- NULL

  # root of the URL to access to graphQL API for ARDECO
  link <- 'https://urban.jrc.ec.europa.eu/ardeco-api-v2/graphql'
  conn <- GraphqlClient$new(url=link)

  # build the graphql query to recover the list of variables
  # and related description

  query <- paste('query {variable(id: "', variable,  '") {nutsLevel, description}}', sep = '')
  new <- Query$new()$query('link', query)

  tryCatch({
    # submit the GraphQL API request
    result <- conn$exec(new$link) %>% fromJSON(flatten = F)
    return(result$data$variable)
  }, error = function(e) {
    strErr <- paste("Error call: ", link)
    stop(strErr)
  })

}
