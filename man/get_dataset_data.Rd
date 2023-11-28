\name{ardeco_get_dataset_data}
\alias{ardeco_get_dataset_data}
\title{ardeco_get_dataset_data}
\usage{
ardeco_get_dataset_data(var_code, unit, sector)
}
\details{
description: return the data frame containing the data related to the input dataset
}
\arguments{
  \item{var_code}{the code of variable}
  \item{unit}{unit of measure}
  \item{sector}{reference sector}
}
\value{
This function return a data frame including the data related to the selected dataset. The data frame include the following fileds:
  \itemize{
    \item{DATE: year of reference of the value}
    \item{SECTOR: sector of referenc of the value}
    \item{TERRITORY_ID: code of the territorial unit of reference. It\'s one of the NUTS code (see EUROSTAT)}
    \item{UNIT: unit of measure of the value}
    \item{VARIABLE: code of the variable}
    \item{value: value of the selected variable related to the date, secotr, territory_id, unit, variable}
  }
}

\examples{
ardeco_get_dataset_data('SNPTN', 'Persons', 'Total')
}
\description{
The function recover the data via API of th dataset specified in input and return the list of data for each year an deach nuts_code in data frame format.
}
