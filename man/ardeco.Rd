\name{ARDECO packages}
\alias{ARDECO packages}
\title{ARDECO packages}
\section{The exposed funtions}{
This package provides three functions which are linked between them and have to be used in the following way.
\itemize{
  \item \bold{ardeco_get_variable_list} - to recover the list of the avavilable variables with related descritpion.
  \item \bold{ardeco_get_dataset_list} - After having identified the variable of interest, it's possible to identify the list of datasets included into the variable of interest. This list of datasets is recovered by using ardeco_get_dataset_list with the variable code of interest.
  \item \bold{ardeco_get_dataset_data} - To recover the data related to a variable it's possibile to do it by using just the variable code (recovering all variable data) or filtering it using unit, sector (recovered by ardeco_get_dataset_list) and/or nuts version, nuts code, year and level.
}
}


\description{
This package provide a set of functions to access the ARDECO data
directly from the official ARDECO public repository through the exploitation
of the ARDECO API. The API are completely transparent to the user and the
provided functions provide a direct access to the ARDECO data.
The ARDECO (Annual Regional Database of the European Commission) database is
a collection of variables related to demography, employment, labour market,
domestic product, capital formation.
Each variable can be exposed in one or more units of measure as well as
refers to total values plus economic sectors (NACE sectors as defined by
EUROSTAT)
The description of the ARDECO database can be found at the following URL
https://urban.jrc.ec.europa.eu/ardeco
}


\examples{
ardeco_get_variable_list()
ardeco_get_dataset_list('SNPTN')
ardeco_get_dataset_data('SNETD', version=2021, level=0, nutscode='IT', year=2020)
}

