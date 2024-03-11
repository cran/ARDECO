# European Commission
# ARDECO database
# R packege "ardeco" exposing ARDECO data to be used in R
#
# Function: ardeco_get_dataset_list
# Input: var_code = variable code
# Output: list of datasets for the requested variable
#         - dt_var: variable code
#         - dt_unit: unit: unit of measure for the dataset
#         - dt_sector: if there is only one sector, this is coded 'Total' otehrwise
#                   it's a NACE sector
#
# Description: return the list of the datasets related to a variable (input parameter).
#              for each dataset is returned the variableCode, the unit of measure and
#              the sector.
#              For variable with just only one sector, this usually is identified
#              by "Total" code.
#
#' @export

ardeco_get_dataset_list <- function(var_code) {

  # binding variables to read dataset list with lastBatchList
  lastBatchList <- NULL

    # root of the URL to access to graphQL API for ARDECO
  link <- 'https://urban.jrc.ec.europa.eu/ardeco-api-v2/graphql'
  conn <- GraphqlClient$new(url=link)

  # build the graphql query to recover the list of dataset for
  # the requested variable
  query <- paste('query {
            datasetList (variableCode: "',var_code,'") {
              variableCode
              unit
              sector
              lastBatchList(release: true) {
                nutsVersion
              }
            }
          }',sep="")
  new <- Query$new()$query('link', query)

  # submit the GraphQL API request
  result <- conn$exec(new$link) %>% fromJSON(flatten = F)

  # convert the result in formatted list
  dataset_list <- result$data$datasetList %>% as_tibble()
  dataset_list <- dataset_list %>% unnest(lastBatchList)

  # rename column name of dataset_list with suffix dt (dt_var, dt_unit, dt_sector)
  lookup <- c(dt_var="variableCode", dt_unit="unit", dt_sector="sector", dt_version="nutsVersion")
  dataset_list = rename(dataset_list, all_of(lookup))

  # return the formatted data
  return(dataset_list)
}
