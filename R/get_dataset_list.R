# European Commission
# ARDECO database
# R packege "ardeco" exposing ARDECO data to be used in R
#
# Function: ardeco_get_dataset_list
# Input: var_code = variable code
# Output: list of datasets for the requested variable
#         - var: variable code
#         - unit: unit: unit of measure for the dataset
#         - version: the available nutcode version
#         - list of additional dimensions like sector, sex, age class, others
#               and, for each of these additional dimensions, the possible values
#
# Description: return the list of the datasets related to a variable (input parameter).
#              for each dataset is returned the variableCode, the unit of measure, the nutscode version
#              and the eventual additional dimensions better detailing the variable
#
#' @import dplyr
#' @import ghql
#' @import httr
#' @import jsonlite
#' @import tidyr
#' @import stringr
#' @export

ardeco_get_dataset_list <- function(var_code) {

  # check if the var_code has been specified
  if (missing(var_code)) {
    print("ERROR: You must specify var_code")
    return(NULL)
  }

  # check if the var_code has one of the permitted values
  variables_available = ardeco_get_variable_list()
  if (!var_code %in% variables_available$code) {
    print(paste("Variable ", var_code, "does not exist. Variables permitted:[", paste(unique(variables_available$code), collapse = ", "), "]"))
    return(NULL)
  }

  # root of the URL to access to graphQL API for ARDECO
  link <- 'https://urban.jrc.ec.europa.eu/ardeco-api-v2/graphql'
  conn <- GraphqlClient$new(url=link)

  # build the graphql query to recover the list of dataset for
  # the requested variable

  query <- paste('query {
            variable (id:"',var_code,'") {
              nutsLevel
              round code
              tolerance
              lastYear
              export
              classificationClassId
              allowedDimensions
              description
              nutsVersionList
              datasets {
                datasetId
                dimensions {key value}}}}', sep="")


  new <- Query$new()$query('link', query)
  # submit the GraphQL API request

  suppressWarnings({

    result <- NULL

    tryCatch({
      result <- conn$exec(new$link) %>% fromJSON(flatten = F)
    }, error = function(e) {
      cat(conditionMessage(e))
    })


    # if API call fails, return error about API availability
    if (is.null(result$data$variable)) {
      print("Error during execution. Check variable value or API avalaibility")
      return(NULL)
    }

    # recover from the API result the list of all information related to the datasets
    dataset_list <- result$data$variable$datasets
    d_nutsVersionList = result$data$variable$nutsVersionList
    d_keys <- lapply(dataset_list$dimensions, "[[", "key")
    d_values <- lapply(dataset_list$dimensions, "[[", "value")

    dataset_dataframe <- data.frame()

    # formatting and return the recovered info related to the dataset list
    for (v in d_values) {
      list_mod <- c(var_code, v)
      list_mod <- c(list_mod, toString(d_nutsVersionList))
      dataset_dataframe <- rbind(dataset_dataframe, list_mod)
    }
    colnames(dataset_dataframe)[1] = 'var'

    for (i in seq_along(d_keys[[1]])) {
      colnames(dataset_dataframe)[i+1] = d_keys[[1]][i]
      colnames(dataset_dataframe)[i+2] = "vers"
    }

    dataset_tibble <- as_tibble(dataset_dataframe)
  })

  return(dataset_tibble)
}
