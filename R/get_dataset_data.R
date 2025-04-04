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
#        tercet_code: (optional) it can be one of the available tercet_code as returned by ardeco_get_tercet_list() function. Tercet_code cannot be defined with level.
#        tercet_class_code: (optional) it can be one of the available tercet_class_code as returned by ardeco_get_tercet_list() function. Tercet_class_code cannot be defined with level.
#        show_perc: (optional). To be used with tercet_code or tercet_class_code. If true, the velue of tercet class are returned as percentage related to the country total.
#        verbose: (optional) if true, it display the current dataset for which the process is fetching data
# Output: a dataframe collecting the data related to the requested dataset
#         detailed by year, nuts_code, unit and sector
#         The fields of the dataframe are the following:
#         - VARIABLE: code of the variable
#         - VERSION: nuts version of the nutscode
#         - LEVEL: the nuts level of nutscode
#         - NUTSCODE: code of the territory (NUTS code) of the value
#         - TERCET_CLASS_CODE: (if required) code of the requested tercet class.
#         - TERCET_CODE: (if required) code of the requested tercet.
#         - YEAR: year of reference of the value
#         - DIM(s): one or mode columns depending by the dimensions defined for the selected dataset
#         - UNIT: unit of measure of the value
#         - TERCET_NAME: (if required) name of the requested tercet.
#         - TERCET_CLASS_NAME: (if required) name of the requested tercet class.
#         - VALUE: value of the variable related to a specific unit, dim(s), version, level, nutscode, year.
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
#' @import arrow
#' @importFrom stats var
#' @importFrom utils URLdecode URLencode
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
    print("Variable is mandatory")
    return(NULL)
  }

  # 	1.1.2) one of the possible variable code
  variables_available = ardeco_get_variable_list()
  if (!variable %in% variables_available$code) {
    print(paste("Variable ", variable, "does not exist"))
    return(NULL)
  }
  # recover the list of available datasets defined into the variable and set the URL with the variable code
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
  tercet_code <- NULL
  tercet_class_code <- NULL
  show_perc <- NULL
  verbose <- NULL
  vers <- NULL
  TERCET_NAME.1 <- NULL
  TERCET_CLASS_NAME.1 <- NULL

  exclude_list <- list()
  call_list <- list()

  # recover the optional parameters passed to the function call
  add_dims <- NULL
  tryCatch({
    add_dims <- list(...)
  }, error = function(e) {
    #print(e)
  })
  if (is.null(add_dims)) {
    print("Error in additional parameters values")
    return(NULL)
  }
  for (param in names(add_dims)) {
    exclude_list <- c(exclude_list, param)
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
    if (param == "tercet_code") {
      tercet_code <- add_dims[param]
    }
    if (param == "tercet_class_code") {
      tercet_class_code <- add_dims[param]
    }
    if (param == "tercet") {
      tercet_class_code <- add_dims[param]
    }
    if (param == "show_perc") {
      show_perc <- add_dims[param]
    }
    if (param == "verbose") {
      verbose <- add_dims[param]
    }

  }


  # check if show_perc has been passed with one of tercet_code or tercet_class_code
  if (!is.null(show_perc)) {
    if (is.null(tercet_class_code) && is.null(tercet_code)) {
      print("You can't specify the show_perc parameter without specifying either tercet_code or tercet_class_cose")
      return(NULL)
    }
  }


  # select the variable's datasets for which fetching data
  df_selected <- datasets_available %>% select(-var, -vers)
  column_names <- colnames(df_selected)
  filtered_column_names <- column_names[!column_names %in% exclude_list]
  num_rows <- nrow(df_selected)
  url_list<- list()
  for (i in 1:num_rows) {
    strTmp <- ""
    for (c in filtered_column_names) {
      if (!is.null(df_selected[[c]]) && length(df_selected[[c]]) > 0) {
        strTmp <- paste(strTmp, "&", sep="")
        strTmp <- paste(strTmp, URLencode(c), sep="")
        strTmp <- paste(strTmp, "=", sep="")
        #strTmp <- paste(strTmp, URLencode(df_selected[[c]][i-1]), sep="")
        strTmp <- paste(strTmp, URLencode(df_selected[[c]][i]), sep="")
      }
    }
    if (substr(strTmp, nchar(strTmp), nchar(strTmp)) != "=") {
      url_list <- c(url_list, strTmp)
    }
  }
  url_list <- unique(url_list)

  # 	1.3) check value of passed optional parameters
  #		1.3.1) For each of ("unit", "version", "nutscode", "year", "level", "tercet") (if passed)
  #	 		check if the passed value is in the domain of the parameter

  if (!is.null(tercet_code) && !is.null(tercet_class_code) ) {
    print("Attention! You can't specify both tercet_class_code and tercet_code. Choose only one of these two parameters.")
    return(NULL)
  }


  # check TERCET_CODE value:
  if (!is.null(tercet_code)) {
    #   - cannot be used together level parameter
    if (!is.null(level)) {
      print("Attention! You can't use both level and tercet. Choose only one of these two parameters.")
      return(NULL)
    }

    #   - can be used if exist data at level3
    if (as.character(nutsLevel) != "3") {
      print(paste("variable", variable , "has no data at level 3 and it's no possible to aggregate data at tercet classes"))
      return(NULL)
    }

    #   - must be one of the tercet_class id returned by ardeco_get_tercet_list
    valid_tercets = ardeco_get_tercet_list()
    tercet_code = as.character((tercet_code))
    tercet_code_list <- strsplit(as.character(tercet_code), ",")[[1]]
    if (length(tercet_code_list) > 1) {
      print("tercet_code must contains only one value")
      return(NULL)
    }
    for (tercet in tercet_code_list) {
      is_present <- any(valid_tercets$tercet_code %in% tercet_code)
      if (!is_present) {
        valid_tercets_list <- sort(unique(unlist(as.list(valid_tercets$tercet_code))))
        print(paste("tercet_code", tercet_code , "is not valid. tercet_code values permitted:[", paste(valid_tercets_list, collapse = ', '), "]"))
        return(NULL)
      }
      url <- paste(url, "&tercet=", sep="")
      url <- paste(url, URLencode(as.character(tercet_code)), sep="")
    }
  }

  # check TERCET_CLASS_CODE value:
  if (!is.null(tercet_class_code)) {
    #   - cannot be used together level parameter
    if (!is.null(level)) {
      print("Attention! You can't use both level and tercet. Choose only one of these two parameters.")
      return(NULL)
    }

    #   - can be used if exist data at level3
    if (as.character(nutsLevel) != "3") {
      print(paste("variable", variable , "has no data at level 3 and it's no possible to aggregate data at tercet classes"))
      return(NULL)
    }

    #   - must be one of the tercet_class id returned by ardeco_get_tercet_list
    valid_tercets = ardeco_get_tercet_list()
    tercet_class_code = as.character((tercet_class_code))
    tercet_list <- strsplit(as.character(tercet_class_code), ",")[[1]]
    if (length(tercet_list) > 1) {
      print("tercet must contains only one value")
      return(NULL)
    }
    for (tercet in tercet_list) {
      is_present <- any(valid_tercets$tercet_class_code %in% tercet_class_code)
      if (!is_present) {
        valid_tercets_list <- sort(unique(unlist(as.list(valid_tercets$tercet_class_code))))
        print(paste("tercet_class_code", tercet_class_code , "is not valid. tercet values permitted:[", paste(valid_tercets_list, collapse = ', '), "]"))
        return(NULL)
      }
      url <- paste(url, "&tercet_class=", sep="")
      url <- paste(url, URLencode(as.character(tercet_class_code)), sep="")
    }
  }


  #	check UNIT value
  if (!is.null(unit)) {
    unit = as.character((unit))
    if (!unit %in% datasets_available$unit) {
      print(paste("unit", unit , "is not valid. units permitted:[", paste(unique(datasets_available$unit), collapse = ", "), "]"))
      return(NULL)
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
      print(strErr)
      return(NULL)
    }
    if (!any(grepl(version, datasets_available$vers))) {
      print(paste("Version", version , "is not valid. versions permitted:[", paste(unique(datasets_available$vers), collapse = ", "), "]"))
      return(NULL)
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
        print(strErr)
        return(NULL)
      }

      str_years = as.character(year)
      if (grepl("-", str_years)) {
        year_list <- strsplit(as.character(year), "-")[[1]]
        if (length(year_list) != 2) {
          print("Illegal value in year parameter")
          return(NULL)
        }
        val_min = as.numeric(year_list[1])
        val_max = as.numeric(year_list[2])

        if ((is.na(as.numeric(val_min)) || is.na(as.numeric(val_max)))) {
          print("Illegal value in year parameter")
          return(NULL)
        }

        if (val_min > val_max) {
          print("Wrong min max in year parameter")
          return(NULL)
        }

        for (y in val_min:val_max) {
          url <- paste(url, "&year=", sep="")
          url <- paste(url, URLencode(as.character(y)), sep="")
        }
      } else {
        year_list <- strsplit(as.character(year), ",")[[1]]
        for (y in year_list) {
          if (is.na(as.numeric(y))) {
            print(paste("Level contains illegal value: ", y))
            return(NULL)
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
        print(strErr)
        return(NULL)
      }

    str_level = as.character(level)
    if (grepl("-", str_level)) {
      level_list <- strsplit(as.character(level), "-")[[1]]
      if (length(level_list) != 2) {
        print("Illegal value in level parameter")
        return(NULL)
      }
      val_min = as.numeric(level_list[1])
      val_max = as.numeric(level_list[2])

      if ((is.na(as.numeric(val_min)) || is.na(as.numeric(val_max)))) {
        print("Illegal value in level parameter")
        return(NULL)
      }

      if (val_min > val_max) {
        print("Wrong min max in level parameter")
        return(NULL)
      }

      for (lev in val_min:val_max) {
        url <- paste(url, "&level_id=", sep="")
        url <- paste(url, URLencode(as.character(lev)), sep="")
      }
    } else {
      level_list <- strsplit(as.character(level), ",")[[1]]
      for (lev in level_list) {
        if (is.na(as.numeric(lev))) {
          print(paste("Level contains illegal value: ", lev))
          return(NULL)
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
    print("Error in additional parameters values")
    return(NULL)
  }
  real_add_dims <- ''
  for (param in names(add_dims)) {

    #	skip already processed parameters
    if (param == "level" || param == "year" || param == "nutscode" || param == "unit" || param == "version" || param == "tercet" || param == "tercet_class_code" || param == "tercet_code" || param == 'show_perc' || param == 'verbose') {
      next
    }

    #	check parameters and related values
    valid_values <- datasets_available[[param]]
    if (is.null(valid_values)) {
      print(paste("There is an invalid dimension...", param))
      return(NULL)
    }

    suppressWarnings({
    strErr <- paste(param, " must be numeric or string!")
    tp <- NULL
    tryCatch({
      tp <- typeof(add_dims[[param]])
    }, error = function(e) {

    })
    if (is.null(tp)) {
      print(strErr)
      return(NULL)
    }
    })


    if (!as.character(add_dims[[param]]) %in% valid_values) {
      dim_input <- as.character(add_dims[[param]])
      print(paste(dim_input,
      "is not a valid value for dimension",
      param,
      "- Values permitted:[", paste(unique(valid_values), collapse = ", "), "]"))
      return(NULL)
    }


    url <- paste(url, "&", sep="")
    url <- paste(url, param, sep="")
    url <- paste(url, "=", sep="")
    url <- paste(url, URLencode(as.character(add_dims[[param]])), sep="")

    real_add_dims <- paste(real_add_dims, "[", sep="")
    real_add_dims <- paste(real_add_dims, param, sep="")
    real_add_dims <- paste(real_add_dims, "]=", sep="")
    real_add_dims <- paste(real_add_dims, as.character(add_dims[[param]]), sep="")
    real_add_dims <- paste(real_add_dims, " ", sep="")

  }

  # 2.1) Build URL to retrieve data
  url <- stringr::str_replace_all(url, "\\?&", "?")

  readfile <- data.frame()

  suppressWarnings({
    #	2.2) request the data by REST API
    readfile <- NULL
    errorMessage <- "Error during execution. Check parameter values or API avalaibility"

    # Build the URL for each dataset to fetch and request the data in parquet format
    for (u in url_list) {
      dataset_info <- ''
      dataset_info <- paste(dataset_info, URLdecode(u), sep="")
      dataset_info <- gsub("&", " [", dataset_info)
      dataset_info <- gsub("=", "]=", dataset_info)

      url_ext <- url
      url_ext <- paste(url_ext, u, sep="")
      url_ext <- paste(url_ext, "&format=parquet", sep="")

      if (!is.null(show_perc)) {
        if (show_perc == TRUE) {
          url_ext <- paste(url_ext, "&show_perc=true", sep="")
        }
      }

      if (!is.null(verbose)) {
        if (verbose == TRUE) {
          message_verbose <- 'Fetching...'
          message_verbose <- paste(message_verbose, real_add_dims, sep="")
          message_verbose <- paste(message_verbose, dataset_info, sep="")
          #message_verbose <- paste(message_verbose, url_ext, sep="")
          message_verbose <- gsub("\\s+", " ", message_verbose)
          print(message_verbose)
        }
      }

      tryCatch({
        #readfile <- read.csv(url)
        readfileTmp <- read_parquet(url_ext)
        readfile <- rbind(readfile, readfileTmp)
      }, error = function(e) {
        print(e)
        errorMessage <- paste(readfileTmp, conditionMessage(e))
      })

    }

    # if there eas an error, exit with error message and NULL data
    if (is.null(readfile)) {
      print(errorMessage)
      return(NULL)
    }

    # Check dataframe empty
    if (nrow(readfile) == 0) {
      print("*** NO DATA ***")
      return(NULL)
    }

    #	2.3) Formatting the output in order to have all fields with the field name according to the output specifications, ie:
    #         - VARIABLE: code of the variable
    #         - VERSION: nuts version of the nutscode
    #         - LEVEL: the nuts level of nutscode
    #         - NUTSCODE: code of the territory (NUTS code) of the value
    #         - TERCET_CLASS_CODE: code of the tercet class of the value (optional)
    #         - TERCET_CODE: code of the tercet code of the value (optional)
    #         - YEAR: year of reference of the value
    #         - DIM(s): one or mode columns depending by the dimensions defined for the selected dataset in lower case
    #         - UNIT: unit of measure of the value
    #         - TERCET_NAME: name of the tercet code of the value (optional)
    #         - TERCET_CLASS_NAME: name of the tercet class of the value (optional)
    #         - VALUE: value of the variable related to a specific unit, dim(s), version, level, nutscode, year
    for (i in 1:length(colnames(readfile))) {

      #print(colnames(readfile)[i])


      tryCatch({
        if (colnames(readfile)[i] == "LEVEL_ID") {
          colnames(readfile)[i] = "LEVEL"
        }
      }, error = function(e) {
        print(e)
      })


      tryCatch({
        if (colnames(readfile)[i] == "TERRITORY_ID") {
          colnames(readfile)[i] = "NUTSCODE"
        }
      }, error = function(e) {
        print(e)
      })

      tryCatch({
        if (colnames(readfile)[i] == "TERCET_CLASS") {
          colnames(readfile)[i] = "TERCET_CLASS_CODE"
        }
      }, error = function(e) {
        print(e)
      })

      tryCatch({
        if (colnames(readfile)[i] == "TERCET") {
          colnames(readfile)[i] = "TERCET_CODE"
        }
      }, error = function(e) {
        print(e)
      })


    }
    # remove DATE and NAME-HTML columns from output table
    readfile <- readfile %>% select(-one_of('DATE', 'NAME_HTML'))
    # readfile <- readfile %>% select(-NAME_HTML)
    # insert VARIABLE column into output table in 1st position

    # Check dataframe empty
    if (nrow(readfile) == 0) {
      print("*** NO DATA ***")
      return(NULL)
    } else {
      if (!is.null(tercet_class_code)) {
        tercet_class_name <- unique(valid_tercets$tercet_class_name[valid_tercets$tercet_class_code == tercet_class_code])
        readfile$TERCET_CLASS_NAME <- tercet_class_name
        col_names <- colnames(readfile)
        readfile <- readfile[, c(col_names[1:(length(col_names)-2)], col_names[length(col_names)], col_names[(length(col_names)-1)])]
        #readfile <- cbind(TERCET = tercet_class_name, readfile)
      }
      readfile <- cbind(VARIABLE = variable, readfile)
    }


    # Check if TERCET or TERCET_CLASS has been requested, include the name for TERCET and TERCET_CLASS
    if (!is.null(tercet_code)) {
      merged_df <- merge(readfile, valid_tercets, by.x = "TERCET_CODE", by.y = "tercet_code", all.x = TRUE)
      merged_df <- merged_df[, c(names(readfile), "tercet_name")]
      merged_df <- merge(merged_df, valid_tercets, by.x = "TERCET_CLASS_CODE", by.y = "tercet_class_code", all.x = TRUE)
      merged_df <- merged_df[, c(names(readfile), "tercet_name.x", "tercet_class_name")]
      df <- merged_df %>% distinct()

      for (i in 1:length(colnames(df))) {
        if (colnames(df)[i] == "tercet_name.x") {
          colnames(df)[i] = "TERCET_NAME"
        }
        if (colnames(df)[i] == "tercet_class_name") {
          colnames(df)[i] = "TERCET_CLASS_NAME"
        }
      }

      cols <- colnames(df)
      value_index <- which(cols == "VALUE")
      new_order <- c(cols[1:(value_index-1)], "TERCET_NAME", "TERCET_CLASS_NAME", "VALUE", cols[(value_index+1):length(cols)])
      df <- df[, new_order]
      df <- df %>% select(-TERCET_NAME.1, -TERCET_CLASS_NAME.1)


      # Return data with requested tercet info in a dataframe structure
      options(max.print = 1000)
      return(df)
    }

    # Return data in dataframe structure
    return (readfile)
    #return (as_tibble(readfile))
  })
}


