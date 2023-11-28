# European Commission
# ARDECO database
# R packege "ardeco" exposing ARDECO data to be used in R
#
# Function: ardeco_get_dataset_data
# Input: var_code = variable code
#        unit: unit of measure of the dataset
#        sector: if there is only one sector, this is coded 'Total', otherwise
#                it's one of the NACE sectors
# Output: a dataframe collecting the data related to the requested dataset
#         detailed by year, nuts_code, unit and sector
#         The fields of the dataframe are the following:
#         - DATE: year of the value
#         - SECTOR: if there is only one sector, this is coded 'Total' otehrwise
#                   it's a NACE sector
#         - TERRITORY_ID: code of the territory (NUTS code) of the value
#         - UNIT: unit of measure of the value
#         - VARIABLE: code of the variable
#         - value: value of the variable related to a specific year, territory, unit, sector
#
# Description: return the list of value for each year and each territory related to
#              the requested dataset specified by variable code, unit and sector (input parameter)
#
#' @export

ardeco_get_dataset_data <- function(var_code, unit, sector) {

  # root of the URL to access to graphQL API for ARDECO
  link <- 'https://urban.jrc.ec.europa.eu/ardeco-api-v2/graphql'
  conn <- GraphqlClient$new(url=link)

  # build the graphql query to recover the batch_id related to
  # the release of the requested dataset
  query <- paste('query { dataset (
                 variableCode: "',var_code,
  						 '",unit: "', unit,
  						 '",sector: "',sector,'") {lastBatch {id }}}',sep="")
  new <- Query$new()$query('link', query)

  # submit the GraphQL API request
  result <- conn$exec(new$link) %>% fromJSON(flatten = F)

  # extract from the result the batch_id to use to recover dataset data
  batch_id <- result$data$dataset$lastBatch$id

  # submit REST API to recover dataset data using the batch_id
  # - Build the API rest request
  call <- paste('https://urban.jrc.ec.europa.eu/ardeco-api-v2/rest/batch/', batch_id,sep="")
  # submit rest API request converting data from JSONstat to R data frame (function fromJSONstat)
  dataset_data <- fromJSONstat(call, naming="id")

  # return the formatted data
  return(dataset_data)
}
