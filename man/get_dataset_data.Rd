\name{ardeco_get_dataset_data}
\alias{ardeco_get_dataset_data}
\title{ardeco_get_dataset_data}
\usage{
ardeco_get_dataset_data(variable, ...)
}
\arguments{
  \item{variable}{mandatory: the code of variable}
  \item{...}{Other optional parameters to filter data according to the different dimensions. The dimensions common to all datasets are: version, level, nutscode, year, unit, tercet_code, tercet_class_code, verbose, show_perc. Additional dimensions can be setup according to the selected varable (see ardeco_get_dataset_list to check the additional dimension for a variable)}
}
\value{
This function return a data frame including the data related to the selected dataset. The data frame include the following fileds:
  \itemize{
    \item{VARIABLE: code of the variable}
    \item{VERSION: nuts version of NUTS code.}
    \item{LEVEL: level of NUTS code. From 0 to 3 represent NUTS0-3 level; 4 refers to Metropolitan regions, 9 refers data at EU level.}
    \item{NUTSCODE: code of the territorial unit of reference. It\'s one of the NUTS code (see EUROSTAT)}
    \item{TERCET_CLASS_CODE: (if required) code of the requested tercet class.}
    \item{TERCET_CODE: (if required) code of the requested tercet }
    \item{YEAR: year of reference of the value.}
    \item{DIM(s): one or mode columns depending by the dimensions defined for the selected dataset}
    \item{UNIT: unit of measure of the value.}
    \item{TERCET_NAME: (if required) the name of the requested tercet }
    \item{TERCET_CLASS_NAME: (if required) the name of the requested tercet class.}
    \item{VALUE: value of the selected variable related to the date, sector, territory_id, unit, variable}
  }
}

\description{
The function recover the data via API of the dataset specified in input applying the optional filters and return the list of data in data frame format. The process retrieve each dataset defined in the selected variable and merge the result into the output dataframe. The optional parameter \bold{show_perc=TRUE} permits to check the progress of the retrieval process listing the datasets in processing.
}
\details{
Each parameter have to be passed using notation \bold{<param-name>=\sQuote{<param-value>}}.

For some Optional parameters it can be used a special notation increasing the filtering options.

FILTERING OPTION FOR PARAMETER \bold{nutscode}

It's possible to require values with nuts codes satisfing multiple conditions, using the character \sQuote{,} to sepatare the different conditions.

For example: \bold{nutscode=\sQuote{EE,IT}} return only the nuts codes related starting with \sQuote{EE} or \sQuote{IT}, i.e. return all values for Estonia and Italy.

FILTERING OPTION FOR PARAMETERS \bold{year} and \bold{level}

The parameters  \bold{year} and \bold{level} are numeric parameters.

A numeric parameter can have a simple value. In this case the function return the values in which the specific parameter is EQUAL to the inserted value.

For example: \bold{level=0} return the values at NUTS0 level.

It's possible to require values satisfing multiple conditions, using the character \sQuote{,} to require values for different year or level.

For example: \bold{level=\sQuote{0,2}} return the values for level 0 and level 2. Remember to use quote to define the list of values.

It's also possible to filter data defining an interval of years/levels. It can be defined using this notation: \bold{\sQuote{min-max}} where min and max are the minimun and maximus values. Remember to use quote to define the values interval.

For example: \bold{year=\sQuote{2000-2005}} return the values for the years starting from 2000 to 2005.

FILTERING OPTION FOR PARAMETERS \bold{tercet_code} and \bold{tercet_class_code}

The parameters \bold{tercet_code} is a numeric parameter corresponding to a tercet name returned by ardeco_get_tercet_list() function.

Using this parameter, the returned values are related to the aggregated data at country level (nuts level 0)
for all the tercet classes defined into the selected tercet.

To retrieve only one tercet class, use the parameter \bold{tercet_class_code}. This is a numeric parameter corresponding to a tercet class name returned by ardeco_get_tercet_list() function.

\bold{tercet_code} and \bold{tercet_class_code} return the absolute values of the requested tercet classes. To retrieve the share of the requested tercet classes, use the optional parameter \bold{show_perc=TRUE}.

The parameters \bold{tercet_code} and \bold{tercet_class_code)} cannot be use together \bold{level}

For example:

\bold{tercet_code=1} return the absolute values at country level related to the tercet classes defined in "Urban-Rural Typology"

\bold{tercet_class_code=0, show_perc=TRUE} return the share at country level related to the tercet class "Predominantly urban"
}
