# European Commission
# ARDECO database
# R packege "ardeco" exposing ARDECO data to be used in R
#
# Function: ardeco_get_dataset_data
# Input: variable (mandatory) = variable code
#        unit: (optional) unit of measure of the dataset. If NULL return all units data
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
#        dim(s): (optional) it can be one or more dimensions of dataset. if no dimension is passed, return data for all existing dimensions.
#        tercet: (optional) it can be one of the available tercet class id as returned by ardeco_get_tercet_list() function. Tercet cannot be defined with level.
# Output: a dataframe collecting the data related to the requested dataset
#         detailed by year, nuts_code, unit and sector
#         The fields of the dataframe are the following:
#         - VARIABLE: code of the variable
#         - VERSION: nuts version of the nutscode
#         - LEVEL: the nuts level of nutscode
#         - NUTSCODE: code of the territory (NUTS code) of the value
#         - YEAR: year of reference of the value
#         - DIM(s): one or mode columns depending by the dimensions defined for the selected dataset
#         - UNIT: unit of measure of the value
#         - TERCET_CLASS_NAME: (if required) name of the requested tercet class name.
#         - VALUE: value of the variable related to a specific unit, dim(s), version, level, nutscode, year
#
# Description: return the list of value for each version, level, year and nutscode related to
#              the requested dataset specified by variable code, unit and dimensions.
#              variable code is a mandatory parameter. All the others input parameter are optional.
#              If it's defined only the variable, the function return all data related the requested variable.

#
#' @import dplyr
#' @import ghql
#' @import httr
#' @import jsonlite
#' @import tidyr
#' @import stringr
#' @import utils
#' @export

ardeco_get_dataset_data <- function(variable,
                                    ...) {

  nutsLevel <- ardeco_get_variable_props(variable)
  nutsLevel <- nutsLevel$nutsLevel

  #	define the URl base of REST API
  url <- "https://urban.jrc.ec.europa.eu/ardeco-api-v2/rest/export/"

  # 1) check validity of input parameters
  # 	1.1) variable parameter
  #		1.1.1) have to be defined
  if (missing(variable)) {
    return("Variable is mandatory")
  }

  # 	1.1.2) one of the possible variable code
  variables_available = ardeco_get_variable_list()
  if (!variable %in% variables_available$code) {
    return(paste("Variable ", variable, "does not exist"))
  }
  datasets_available = ardeco_get_dataset_list(variable)
  url <- paste(url, variable, sep="")
  url <- paste(url, "?" , sep="")

  # 	1.2) check the validity of all other optional parameters.
  #		1.2.1) one of "unit", "version", "nutscode", "year", "level"
  # 		1.2.2) one of the possible dimensions as defined in the selected variable
  unit <- NULL
  version <- NULL
  nutscode <- NULL
  year <- NULL
  level <- NULL
  tercet <- NULL

  # recover the optional parameters passed to the function call
  add_dims <- NULL
  tryCatch({
    add_dims <- list(...)
  }, error = function(e) {
    #print(e)
  })
  if (is.null(add_dims)) {
    return("Error in additional parameters values")
  }
  for (param in names(add_dims)) {
    if (param == "year") {
      year <- add_dims[param]
    }
    if (param == "nutscode") {
      nutscode <- add_dims[param]
    }
    if (param == "version") {
      version <- add_dims[param]
    }
    if (param == "level") {
      level <- add_dims[param]
    }
    if (param == "unit") {
      unit <- add_dims[param]
    }
    if (param == "tercet") {
      tercet <- add_dims[param]
    }

  }

  # 	1.3) check value of passed optional parameters
  #		1.3.1) For each of ("unit", "version", "nutscode", "year", "level", "tercet") (if passed)
  #	 		check if the passed value is in the domain of the parameter

  # check TERCET value:
  if (!is.null(tercet)) {
    #   - cannot be used together level parameter
    if (!is.null(level)) {
      return("Attention! You can't use both level and tercet. Choose only one of these two parameters.")
    }

    #   - can be used if exist data at level3
    if (as.character(nutsLevel) != "3") {
      return(paste("variable", variable , "has no data at level 3 and it's no possible to aggregate data at tercet classes"))
    }

    #   - must be one of the tercet_class id returned by ardeco_get_tercet_list
    valid_tercets = ardeco_get_tercet_list()
    tercet = as.character((tercet))
    tercet_list <- strsplit(as.character(tercet), ",")[[1]]

    if (length(tercet_list) > 1) {
      return("tercet must contains only one value")
    }

    for (tercet in tercet_list) {
      is_present <- any(valid_tercets$tercet_class_code %in% tercet)
      if (!is_present) {
        valid_tercets_list <- sort(unique(unlist(as.list(valid_tercets$tercet_class_code))))
        return(paste("tercet", tercet , "is not valid. tercet values permitted:[", paste(valid_tercets_list, collapse = ', '), "]"))
      }
      url <- paste(url, "&tercet_class=", sep="")
      url <- paste(url, URLencode(as.character(tercet)), sep="")
    }
  }


  #	check UNIT value
  if (!is.null(unit)) {
    unit = as.character((unit))
    if (!unit %in% datasets_available$unit) {
      return(paste("unit", unit , "is not valid. units permitted:[", paste(unique(datasets_available$unit), collapse = ", "), "]"))
    }
    url <- paste(url, "&unit=", sep="")
    url <- paste(url, URLencode(as.character(unit)), sep="")
  }


  #	check VERSION values
  if (!is.null(version)) {
    strErr <- "VERSION must be numeric or string!"
    tp <- NULL
    tryCatch({
      tp <- typeof(version)
    }, error = function(e) {

    })
    if (is.null(tp)) {
      return(strErr)
    }
    if (!any(grepl(version, datasets_available$vers))) {
      return(paste("Version", version , "is not valid. versions permitted:[", paste(unique(datasets_available$vers), collapse = ", "), "]"))
    }
    url <- paste(url, "&version=", sep="")
    url <- paste(url, URLencode(as.character(version)), sep="")
  }

  #	check NUTSCODE values
  #	 		1.3.1.2) for nutscode it can be define more values separated by comma
  #	 			ex: nutscode=IT,ES ==> requested nutscode: IT and ES
  if (!is.null(nutscode)) {
    nutscode = as.character((nutscode))
    nutscode_list <- strsplit(as.character(nutscode), ",")[[1]]
    for (nutscode in nutscode_list) {
      url <- paste(url, "&territory_id=", sep="")
      url <- paste(url, URLencode(as.character(nutscode)), sep="")
    }
  }

  #	 		1.3.1.1) for level and year parameters it can be define more values in two main ways:
  #	 			values separated by comma: ex: level=0,2 ==> requested level 0 and level 2
  #	 			2 values separated by -: ex: level=0-2 ==> requested levels: 0, 1 and 2
  #	 Check YEAR values
  if (!is.null(year)) {
      suppressWarnings({
      strErr <- "YEAR must be numeric or string!"
      tp <- NULL
      tryCatch({
        tp <- typeof(year)
      }, error = function(e) {

      })
      if (is.null(tp)) {
        return(strErr)
      }

      str_years = as.character(year)
      if (grepl("-", str_years)) {
        year_list <- strsplit(as.character(year), "-")[[1]]
        if (length(year_list) != 2) {
          return ("Illegal value in year parameter")
        }
        val_min = as.numeric(year_list[1])
        val_max = as.numeric(year_list[2])

        if ((is.na(as.numeric(val_min)) || is.na(as.numeric(val_max)))) {
          return ("Illegal value in year parameter")
        }

        if (val_min > val_max) {
          return ("Wrong min max in year parameter")
        }

        for (y in val_min:val_max) {
          url <- paste(url, "&year=", sep="")
          url <- paste(url, URLencode(as.character(y)), sep="")
        }
      } else {
        year_list <- strsplit(as.character(year), ",")[[1]]
        for (y in year_list) {
          if (is.na(as.numeric(y))) {
            return(paste("Level contains illegal value: ", y))
          }
          url <- paste(url, "&year=", sep="")
          url <- paste(url, URLencode(as.character(y)), sep="")
        }
      }
    })


  }

  #	 Check LEVEL values
  if (!is.null(level)) {
      suppressWarnings({
      strErr <- "LEVEL must be numeric or string!"
      tp <- NULL
      tryCatch({
        tp <- typeof(level)
      }, error = function(e) {

      })
      if (is.null(tp)) {
        return(strErr)
      }

    str_level = as.character(level)
    if (grepl("-", str_level)) {
      level_list <- strsplit(as.character(level), "-")[[1]]
      if (length(level_list) != 2) {
        return ("Illegal value in level parameter")
      }
      val_min = as.numeric(level_list[1])
      val_max = as.numeric(level_list[2])

      if ((is.na(as.numeric(val_min)) || is.na(as.numeric(val_max)))) {
        return ("Illegal value in level parameter")
      }

      if (val_min > val_max) {
        return ("Wrong min max in level parameter")
      }

      for (lev in val_min:val_max) {
        url <- paste(url, "&level_id=", sep="")
        url <- paste(url, URLencode(as.character(lev)), sep="")
      }
    } else {
      level_list <- strsplit(as.character(level), ",")[[1]]
      for (lev in level_list) {
        if (is.na(as.numeric(lev))) {
          return(paste("Level contains illegal value: ", lev))
        }
        url <- paste(url, "&level_id=", sep="")
        url <- paste(url, URLencode(as.character(lev)), sep="")
      }
    }
    })

  }

  #		1.3.2) For each passed dimension
  #	 		check if the passed value is in the domain of the related dimension
  #	 		1.3.1.2) for each dimension it can be define more values separated by comma:
  #	 			ex: sex=male,felame ==> requested sex dimension: male and female
  add_dims <- NULL
  tryCatch({
    add_dims <- list(...)
  }, error = function(e) {
    #print(e)
  })

  if (is.null(add_dims)) {
    return("Error in additional parameters values")
  }

  for (param in names(add_dims)) {

    #	skip already processed parameters
    if (param == "level" || param == "year" || param == "nutscode" || param == "unit" || param == "version" || param == "tercet") {
      next
    }

    #	check parameters and related values
    valid_values <- datasets_available[[param]]
    if (is.null(valid_values)) {
      return(paste("There is an invalid dimension...", param))
    }

    suppressWarnings({
    strErr <- paste(param, " must be numeric or string!")
    tp <- NULL
    tryCatch({
      tp <- typeof(add_dims[[param]])
    }, error = function(e) {

    })
    if (is.null(tp)) {
      return(strErr)
    }
    })


    if (!as.character(add_dims[[param]]) %in% valid_values) {
      dim_input <- as.character(add_dims[[param]])
      return(paste(dim_input,
      "is not a valid value for dimension",
      param,
      "- Values permitted:[", paste(unique(valid_values), collapse = ", "), "]"))
    }


    url <- paste(url, "&", sep="")
    url <- paste(url, param, sep="")
    url <- paste(url, "=", sep="")
    url <- paste(url, URLencode(as.character(add_dims[[param]])), sep="")
  }

  # 2.1) Build URL to retrive data
  url <- stringr::str_replace_all(url, "\\?&", "?")
  options(max.print = 100)
  # print (url)
  suppressWarnings({
    #	2.2) request the data by REST API
    readfile <- NULL
    errorMessage <- "Error during execution. Check parameter values or API avalaibility"
    tryCatch({
      readfile <- read.csv(url)
    }, error = function(e) {
      errorMessage <- paste(readfile, conditionMessage(e))
    })

    if (is.null(readfile)) {
      return(errorMessage)
    }


    #	2.3) Formatting the output in order to have all fields with the field name according to the output specifications, ie:
    #         - VARIABLE: code of the variable
    #         - VERSION: nuts version of the nutscode
    #         - LEVEL: the nuts level of nutscode
    #         - NUTSCODE: code of the territory (NUTS code) of the value
    #         - YEAR: year of reference of the value
    #         - DIM(s): one or mode columns depending by the dimensions defined for the selected dataset in lower case
    #         - UNIT: unit of measure of the value
    #         - VALUE: value of the variable related to a specific unit, dim(s), version, level, nutscode, year
    for (i in 1:length(colnames(readfile))) {
      if (colnames(readfile)[i] == "LEVEL_ID") {
        colnames(readfile)[i] = "LEVEL"
      }
      if (colnames(readfile)[i] == "TERRITORY_ID") {
        colnames(readfile)[i] = "NUTSCODE"
      }
    }
    # remove DATE and NAME-HTML columns from output table
    readfile <- readfile %>% select(-one_of('DATE', 'NAME_HTML'))
    # readfile <- readfile %>% select(-NAME_HTML)
    # insert VARIABLE column into output table in 1st position

    # Check dataframe empty
    if (nrow(readfile) == 0) {
      return("*** NO DATA ***")
    } else {
      if (!is.null(tercet)) {
        tercet_class_name <- unique(valid_tercets$tercet_class_name[valid_tercets$tercet_class_code == tercet])
        readfile$TERCET_CLASS_NAME <- tercet_class_name
        col_names <- colnames(readfile)
        readfile <- readfile[, c(col_names[1:(length(col_names)-2)], col_names[length(col_names)], col_names[(length(col_names)-1)])]
        #readfile <- cbind(TERCET = tercet_class_name, readfile)
      }
      readfile <- cbind(VARIABLE = variable, readfile)



    }

    #readfile <- cbind ("VARIABLE" =  variable, readfile)
    return (readfile)
    #return (as_tibble(readfile))
  })
}


