# European Commission
# ARDECO database
# R packege "ardeco" exposing ARDECO data to be used in R
#
# Function: ardeco_get_dataset_data
# Input: variable (mandatory) = variable code
#        unit: (optional) unit of measure of the dataset. If NULL return all units data
#        sector: (optional) if there is only one sector, this is coded 'Total', otherwise
#                it's one of the NACE sectors. If NULL return all sector data
#        version: (optional). If defined return only version belonging into the provided set.
#                 If NULL return all versions
#        nutscode: (optional). If defined return only nuts codes including values defined by the provided regex
#                  If NULL return all nutscodes
#        year: (optional). If defined return only year codes like the listed ones
#              If NULL return all years
#        level: (optional). If defined return only nutscodes belonging into the provided nuts level set
#               The nuts level is an integer code
#               - 0-3: nuts level from nuts0 to nuts3
#               - 4: metro regions
#               - 9: EU average (eg. EU27_2020)
#               If NULL return all levels
# Output: a dataframe collecting the data related to the requested dataset
#         detailed by year, nuts_code, unit and sector
#         The fields of the dataframe are the following:
#         - variable: code of the variable
#         - sector: if there is only one sector, this is coded 'Total' otherwise
#                   it's a NACE sector
#         - unit: unit of measure of the value
#         - level: the nuts level of nutscode
#         - version: nuts version of the nutscode
#         - nutscode: code of the territory (NUTS code) of the value
#         - year: year of reference of the value
#         - value: value of the variable related to a specific unit, sector, version, level, nutscode, year
#
# Description: return the list of value for each version, level, year and nutscode related to
#              the requested dataset specified by variable code, unit and sector.
#              variable code is a mandatory parameter. All the others input parameter are optional.
#              If it's defined only the variable, the function return all data related the requested variable.
#
#' @import dplyr
#' @import ghql
#' @import httr
#' @import jsonlite
#' @import tidyr
#' @importFrom rjstat fromJSONstat
#' @export

ardeco_get_dataset_data <- function(variable,
                                    unit=NULL,
                                    sector=NULL,
                                    version=NULL,
                                    nutscode=NULL,
                                    year=NULL,
                                    level=NULL) {

  # define variables exposed by external functions
  # binding variables to read dataset list with lastBatchList
  dt_unit <- dt_sector <- dt_version <- lastBatchList <- NULL
  # binding variables: api jsonstat data
  DATE <- LEVEL <- TERRITORY_ID <- NULL

  # root of the URL to access to graphQL API for ARDECO
  link <- 'https://urban.jrc.ec.europa.eu/ardeco-api-v2/graphql'
  conn <- GraphqlClient$new(url=link)

  ### 1) read all datasets for variable with batch_id
  ############### START: ardeco_get_dataset_list code requiring also lastBatch:id #######
  # root of the URL to access to graphQL API for ARDECO
  link <- 'https://urban.jrc.ec.europa.eu/ardeco-api-v2/graphql'
  conn <- GraphqlClient$new(url=link)

  # build the graphql query to recover the list of dataset for
  # the requested variable
  query <- paste('query {
            datasetList (variableCode: "',variable,'") {
              variableCode
              unit
              sector
              lastBatchList(release: true) {
                nutsVersion,
                id
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
  lookup <- c(dt_var="variableCode", dt_unit="unit", dt_sector="sector", dt_version="nutsVersion", dt_id="id")
  dataset_list = rename(dataset_list, all_of(lookup))
  ############### END: ardeco_get_dataset_list code requiring also lastBatch:id #######

  # if dataset_list is empty, return
  if (nrow(dataset_list) == 0) {
    return(paste("Variable", variable, "doesn't exist or return no data", sep=" "))
  }

  ### 2) filter datasets list according to unit value
  if ( !is.null(unit)) {
    dataset_list <- dataset_list %>% filter(dt_unit %in% unit)
  }

  # if dataset_list is empty, return
  if (nrow(dataset_list) == 0) {
    return(paste("Unit", unit, "doesn't exist in variable", variable, sep=" "))
  }

  ### 3) filter datasets list according to sector value
  if ( !is.null(sector)) {
    dataset_list <- dataset_list %>% filter(dt_sector %in% sector)
  }

  # if dataset_list is empty, return
  if (nrow(dataset_list) == 0) {
    return(paste("Sector", sector, "doesn't exist in variable", variable, sep=" "))
  }

  ### 4) filter datasets list according to version value
  if ( !is.null(version)) {
    dataset_list <- dataset_list %>% filter(dt_version %in% version)
  }

  # if dataset_list is empty, return
  if (nrow(dataset_list) == 0) {
    return(paste("version", version, "doesn't exist for the selected variable/unit/sector", sep=" "))
  }

  ### 5) read data for all selected dataset and filter by YEAR and NUTSCODE
  # For each identified dataset, recover the data using the lastBatchId
  for (i in 1:nrow(dataset_list)) {


    ## 5.1) read data using API
    message(paste("Recovering dataset: var", variable,"unit:", dataset_list[i,c("dt_unit")], "sector:",dataset_list[i,c("dt_sector")], "version:",dataset_list[i,c("dt_version")]))

    # submit REST API to recover dataset data using the batch_id
    # - Build the API rest request
    call <- paste('https://urban.jrc.ec.europa.eu/ardeco-api-v2/rest/batch/', dataset_list[i,c("dt_id")],sep="")

    # submit rest API request converting data from JSONstat to R data frame (function fromJSONstat)
    json_data <- fromJSONstat(call, naming="id")

    # add 'version' column to read dataframe (version is not returned by API)
    json_data['version'] <- dataset_list[i,c("dt_version")]

    #Store recovered data into a unique data.frame (dataset_data). Assign (first round) or add (rbind) into dataset_data
    if (i == 1) {
      dataset_data <- json_data
    } else {
      dataset_data <- rbind(dataset_data, json_data)
    }

    ## 5.2) Apply filter by nuts
    if ( !is.null(nutscode)) {
      #dataset_data <- data.frame(filter(dataset_data, grepl(nutscode, TERRITORY_ID, ignore.case = TRUE)))
      dataset_data <- dataset_data %>% filter(grepl(nutscode, TERRITORY_ID, ignore.case = TRUE))
      #dataset_data <- dataset_data %>% filter(TERRITORY_ID %in% nutscode)
    }

    ## 5.3) Apply filter by year
    if ( !is.null(year)) {
      dataset_data <- dataset_data %>% filter(DATE %in% year)
    }
  }

  # add column LEVEL to the output data.frame
  # level definition:
  #   nuts0-3: level 0-3
  #   metro: level 4
  #   EU27_2020: level 9
  if (nrow(dataset_data) > 0) {
    dataset_data$LEVEL <- nchar(dataset_data$TERRITORY_ID) - 2
    #Set LEVEL = NULL for NUTS EUR27_2020 (length = 7 i.e. > 3)
    if (nrow(subset(dataset_data, LEVEL > 6)) > 0) {
      dataset_data[dataset_data$LEVEL>6,]$LEVEL <- -1
    }
    if (nrow(subset(dataset_data, LEVEL > 3)) > 0) {
      dataset_data[dataset_data$LEVEL>3,]$LEVEL <- 4
    }
    if (nrow(subset(dataset_data, LEVEL < 0)) > 0) {
      dataset_data[dataset_data$LEVEL<0,]$LEVEL <- 9
    }
  }

  ### 6) Apply filter by LEVEL
  if ( nrow(dataset_data) > 0 & !is.null(level)) {
    dataset_data <- dataset_data %>% filter(LEVEL %in% level)
  }

  ### 7) formatting output data.frame
  if (nrow(dataset_data) > 0) {
    # rename the column for output dataset_data
    lookup <- c(year="DATE", sector="SECTOR", nutscode="TERRITORY_ID", unit="UNIT", variable="VARIABLE", level="LEVEL")
    dataset_data = rename(dataset_data, all_of(lookup))

    # reorder the columns of output dataset_data
    dataset_data <- dataset_data[, c("variable", "sector", "unit", "version", "level", "nutscode", "year", "value")]
  }
  else {
    dataset_data <- data.frame(matrix(ncol=7, nrow=0))
    colnames(dataset_data) <- c("variable", "sector", "unit", "version", "level", "nutscode", "year", "value")
  }

  ### 7) return requested data
  return(dataset_data)
}
