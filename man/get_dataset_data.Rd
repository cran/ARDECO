\name{ardeco_get_dataset_data}
\alias{ardeco_get_dataset_data}
\title{ardeco_get_dataset_data}
\usage{
ardeco_get_dataset_data(variable, unit, sector, version, nutscode, year, level)
}
\arguments{
  \item{variable}{mandatory: the code of variable}
  \item{unit}{optional: unit of measure. If not defined, return all units data}
  \item{sector}{optional: reference sector. If not defined, return all sectors data}
  \item{version}{optional: filter data for inserted nuts version. It can be composed as a set of values (see details). If not defined, return data for all available nuts versions}
  \item{nutscode}{optional: filter data for inserted nutscode, as defined by Eurostat. It can be a regular expression refining the searching criteria (see details). If not defined, return data for all available nutscode}
  \item{year}{optional: filter data for inserted year. It can be composed as a set of values (see details). If not defined, return data for all available year}
  \item{level}{optional: filter data for inserted levels. It can be composed as a set of values (see details). If not defined, return data for all available levels}
}
\value{
This function return a data frame including the data related to the selected dataset. The data frame include the following fileds:
  \itemize{
    \item{variable: code of the variable}
    \item{sector: sector of referenc of the value.}
    \item{unit: unit of measure of the value.}
    \item{version: nuts version of NUTS code.}
    \item{level: level of NUTS code. From 0 to 3 represent NUTS0-3 level; 4 refers to Metropolitan regions, 9 refers data at EU level.}
    \item{nutscode: code of the territorial unit of reference. It\'s one of the NUTS code (see EUROSTAT)}
    \item{year: year of reference of the value.}
    \item{value: value of the selected variable related to the date, sector, territory_id, unit, variable}
  }
}

\examples{
ardeco_get_dataset_data('SNPTN', version=2021, level=0, nutscode='IT', year=2020)
ardeco_get_dataset_data('SNETD', version=2021, level=c(0:1,9), nutscode='^IT|^EU')
}
\description{
The function recover the data via API of the dataset specified in input applying the optional filters and return the list of data in data frame format.
}
\details{
Optional parameters can be inserted using a special notation increasing the filtering options.

FILTERING OPTION FOR PARAMETER \bold{nutscode}

It can be a simple string. In this case the function return all nuts codes including the inserted string.

For example: \bold{nutscode=\sQuote{EE}} return the values having into the nuts code the string \sQuote{EE}, i.e. all the nuts codes starting with \sQuote{EE} but also the nutscodes like \sQuote{DEE...}

It's possible to require the values with the nuts codes starting with a specific string, inserting the character \sQuote{^} at the beginning of the filter.

For example: \bold{nutscode=\sQuote{^EE}} return the values with nuts code starting with \sQuote{EE}

In addition it's possible to require values with nuts codes satisfing multiple conditions, using the character \sQuote{|} to sepatare the different conditions.

For example: \bold{nutscode=\sQuote{^EE|^IT}} return only the nuts codes related starting with \sQuote{EE} or \sQuote{IT}, i.e. return all values for Estonia and Italy.

FILTERING OPTION FOR PARAMETERS \bold{version year level}

The parameters \bold{version}, \bold{year} and \bold{level} are numeric parameters.

A numeric parameter can have a simple value. In this case the function return the values in which the specific parameter is EQUAL to the inserted value.

For example: \bold{level=0} return the values at NUTS0 level.

To filter data for one value to another, it can be defined using this notation: \bold{c(}min\bold{:}max\bold{)} where min and max are the minimun and maximus values.

For example: \bold{year=c(2000:2005)} return the values for the years starting from 2000 to 2005.

It's also possible to combine the two notations in this way: \bold{year=c(2005, 2008:2010)} return the values for the year 2005 and for the year from 2008 and 2010.
}
