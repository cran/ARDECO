\name{ardeco_get_dataset_data}
\alias{ardeco_get_dataset_data}
\title{ardeco_get_dataset_data}
\usage{
ardeco_get_dataset_data(variable, ...)
}
\arguments{
  \item{variable}{mandatory: the code of variable}
  \item{...}{Other optional parameters to filter according to the different dimensions. This can be: version, level, nutscode, year, unit and the additional dimensions depending by the selected varable}
}
\value{
This function return a data frame including the data related to the selected dataset. The data frame include the following fileds:
  \itemize{
    \item{VARIABLE: code of the variable}
    \item{VERSION: nuts version of NUTS code.}
    \item{LEVEL: level of NUTS code. From 0 to 3 represent NUTS0-3 level; 4 refers to Metropolitan regions, 9 refers data at EU level.}
    \item{NUTSCODE: code of the territorial unit of reference. It\'s one of the NUTS code (see EUROSTAT)}
    \item{YEAR: year of reference of the value.}
    \item{DIM(s): one or mode columns depending by the dimensions defined for the selected dataset}
    \item{UNIT: unit of measure of the value.}
    \item{VALUE: value of the selected variable related to the date, sector, territory_id, unit, variable}
  }
}

\examples{
ardeco_get_dataset_data('SNPTN', version=2021, level=0, nutscode='IT', year=2020, sex='Males')
ardeco_get_dataset_data('SUVGD', version=2021, level='0,2', nutscode='EE', year='2018-2020')
}
\description{
The function recover the data via API of the dataset specified in input applying the optional filters and return the list of data in data frame format.
}
\details{
Each parameter have to be passed using notation \bold{<param-name>=\sQuote{<param-value>}}.

For some Optional parameters it can be used a special notation increasing the filtering options.

FILTERING OPTION FOR PARAMETER \bold{nutscode}

It's possible to require values with nuts codes satisfing multiple conditions, using the character \sQuote{,} to sepatare the different conditions.

For example: \bold{nutscode=\sQuote{EE,IT}} return only the nuts codes related starting with \sQuote{EE} or \sQuote{IT}, i.e. return all values for Estonia and Italy.

FILTERING OPTION FOR PARAMETERS \bold{year} and \bold{level}

The parameters \bold{year} and \bold{level} are numeric parameters.

A numeric parameter can have a simple value. In this case the function return the values in which the specific parameter is EQUAL to the inserted value.

For example: \bold{level=0} return the values at NUTS0 level.

It's possible to require values satisfing multiple conditions, using the character \sQuote{,} to require values for different year or level.

For example: \bold{level=\sQuote{0,2}} return the values for level 0 and level 2. Remember to use quote to define the list of values.

It's also possible to filter data defining an interval of years/levels. It can be defined using this notation: \bold{c(}min\bold{-}max\bold{)} where min and max are the minimun and maximus values. Remember to use quote to define the values interval.

For example: \bold{year=\sQuote{2000-2005}} return the values for the years starting from 2000 to 2005.
}
