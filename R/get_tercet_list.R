get# European Commission
# ARDECO database
# R packege "ardeco" exposing ARDECO data to be used in R
#
# Function: ardeco_get_tercet_list
# Input: variable code (optional)
# Output: list of tercet and related tercet classes for which is possible to
#         aggregate data.
#         - tercet_code: Code of tercet: For example URT (Urban-Rural Typologies)
#         - tercet_name: detailed name of tercet: For example Urban-Rural Typologies
#         - tercet_class_code: code of the tercet class
#         - tercet_class_name: name of the tercet class. For example "Predominantly Urban"
#
# Description: return the list of the tercet with the related info (code, name, classes)
#              for which is possible to agregate variables data.
#              If a variable code is passed, the function returns the tercet classes list
#              possible for the selected variable. In general, it's possible to aggregate
#              data at tercet classes if the variable have complete dataset at nuts level 3
#
#' @import dplyr
#' @import ghql
#' @import httr
#' @import jsonlite
#' @import tidyr
#' @import stringr
#' @importFrom stats var
#' @importFrom utils URLdecode URLencode
#' @export

ardeco_get_tercet_list <- function(var_code) {

  # check if the var_code has been specified
  if (!missing(var_code)) {

    # check if the var_code has one of the permitted values
    variables_available = ardeco_get_variable_list()
    if (!var_code %in% variables_available$code) {
      print(paste("Variable ", var_code, "does not exist. Variables permitted:[", paste(unique(variables_available$code), collapse = ", "), "]"))
      return(NULL)
    }

    # check if the var_code have data at nuts level 3
    nutsLevel <- ardeco_get_variable_props(var_code)
    nutsLevel <- nutsLevel$nutsLevel
    if (as.character(nutsLevel) != "3") {
      print(paste("The variable", var_code , "has no data at level 3 and it's no possible to aggregate data at tercet classes"))
      return(NULL)
    }

  }

    # build API request to recover tercet_class_list
    query <- '{
          "query": "{territorialTercetList (territorialLevelId: 3) {id name territorialTercetClassList {id name}}}"
          }'
    # URL endpoint GraphQL
    url <- 'https://urban.jrc.ec.europa.eu/api/graphql'
    response <- POST(url,
                     add_headers(.headers = c("Content-Type" = "application/json")),
                     body = query,
                     encode = "json")
    if (status_code(response) == 200) {
      data <- content(response, as = "parsed", type = "application/json")
    } else {
      print(paste("Error:", status_code(response), response))
      return(NULL)
    }

    # format output and return the tercet list
    df_list <- lapply(data$data$territorialTercetList, function(group) {
      data.frame(
        tercet_code = group$id,
        tercet_name = URLdecode(group$name),
        tercet_class_code = sapply(group$territorialTercetClassList, function(x) x$id),
         tercet_class_name = sapply(group$territorialTercetClassList, function(x) URLdecode(x$name))
       )
    })
    df_list <- bind_rows(df_list)
    return(df_list)


}
