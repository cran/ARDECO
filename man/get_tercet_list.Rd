\name{ardeco_get_tercet_list}
\alias{ardeco_get_tercet_list}
\title{ardeco_get_tercet_list}
\usage{
ardeco_get_tercet_list(var_code)
}
\arguments{
  \item{var_code}{OPTIONAL - one of the code returned by ardeco_get_variable_list()}
}
\value{
The list of tercet and related tercet classes for which is possible to aggregate data.
- tercet_code: Code of tercet: For example URT (Urban-Rural Typologies)
- tercet_name: detailed name of tercet: For example Urban-Rural Typologies
- tercet_class_code: code of the tercet class
- tercet_class_name: name of the tercet class. For example "Predominantly Urban"}

\examples{
ardeco_get_tercet_list()
ardeco_get_tercet_list('SNPTN')
}
\description{
The function return the list of the tercet with the related tercet classes
for which is possible to agregate variables data.
If a variable code is passed, the function returns the tercet classes list
for which it's possibile to aggregate data for the selected variable.
In general, it's possible to aggregate data at tercet classes if the variable
have data at nuts3 level}
