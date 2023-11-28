\name{ardeco_get_dataset_list}
\alias{ardeco_get_dataset_list}
\title{ardeco_get_dataset_list}
\usage{
ardeco_get_dataset_list(var_code)
}
\arguments{
  \item{var_code}{one of the code returned by ardeco_get_variable_list()}
}
\value{
The set of datasets related to the selected variable. Each datraset is described by:
- variable_code
- unit of measure
- sector
}

\examples{
ardeco_get_dataset_list('SNPTN')
}
\description{
The function return the list of dataset linked to a variable through the ARDECO API by defining the varaible code.
For each dataset it'ìs returned the code of variable, the unit fo measure and the sector of reference.
}
