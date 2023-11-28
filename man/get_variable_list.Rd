\name{ardeco_get_variable_list}
\alias{ardeco_get_variable_list}
\title{ardeco_get_variable_list}
\usage{
ardeco_get_variable_list()
}
\value{
This function returns the list of the code and the description of each available variables. The code have to be used in the next functions to recover the datasets and the data values
}
\details{
return the list of the available variables exposed by ARDECO database
}

\examples{
ardeco_get_variable_list()
}
\description{
This function return the list of all available ARDECO variable recovered through the ARDECO API. The function returns the list of code and description of each variable. Code will be used to recover the list of datasets and also the data of a variable.
}
