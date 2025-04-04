## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ARDECO)

## -----------------------------------------------------------------------------
print(ardeco_get_variable_list(), n=1000)

## -----------------------------------------------------------------------------
print(ardeco_get_dataset_list('SNPTN'), n=100)

## -----------------------------------------------------------------------------
mytb <- ardeco_get_variable_list()
print(mytb, max=50)

## -----------------------------------------------------------------------------
mytb <- ardeco_get_dataset_list('SNETZ')
print(mytb)

## -----------------------------------------------------------------------------
mytb <- ardeco_get_tercet_list()
print(mytb)

## -----------------------------------------------------------------------------
mytb <- ardeco_get_tercet_list('RPDTN')

## -----------------------------------------------------------------------------
mydf <- ardeco_get_dataset_data('SNPTD')
print(mydf, max=50)

## -----------------------------------------------------------------------------
mydf <- ardeco_get_dataset_data('SNETZ', verbose=TRUE)

## -----------------------------------------------------------------------------
# Request data for Italy at level 1, at nuts version=2021,
# with unit of measure = 'Million EUR' and related to year 2015
mydf <- ardeco_get_dataset_data('SUVGD', version=2021, unit='Million PPS', year=2015, nutscode='IT', level=1)
print(mydf, max=50)

## -----------------------------------------------------------------------------
# Check the available versions for variable SNPTD
ardeco_get_dataset_list('SNPTD')

mydf <- ardeco_get_dataset_data('SNPTD', version=2021)
print(mydf, max=50)

## -----------------------------------------------------------------------------
# Retrive data with nutscode starting with 'IT'
mydf <- ardeco_get_dataset_data('SUVGD', nutscode='IT')
print(mydf, max=50)

# Retrive data with nutscode starting with 'IT,FR'
mydf <- ardeco_get_dataset_data('SUVGD', nutscode='IT,FR')
print(mydf, max=50)

## -----------------------------------------------------------------------------
# Retrive SUVGD data for Italy at country level (level 0) for the year 2015
mydf <- ardeco_get_dataset_data('SUVGD', level=0, nutscode='IT', year=2015, version=2021)
print(mydf, max=50)

# Retrive SUVGD data for Italy at country and regional level (level 0 and 2) for the year 2015
mydf <- ardeco_get_dataset_data('SUVGD', level='0,2', nutscode='IT', year=2015, version=2021)
print(mydf, max=50)

# Retrive SUVGD data for Italy at level from 0 to 2 for the year 2015
mydf <- ardeco_get_dataset_data('SUVGD', level='0-2', nutscode='IT', year=2015, version=2021)
print(mydf, max=100)


## -----------------------------------------------------------------------------
# Check the available units for variable SUVGD
ardeco_get_dataset_list('SUVGD')

# Retrive data only for unit = 'Million PPS'
mydf <- ardeco_get_dataset_data('SUVGD', unit='Million PPS')
print(mydf, max=50)

## -----------------------------------------------------------------------------
# Retrive data only for year = 2015
mydf <- ardeco_get_dataset_data('SNPTD', year=2015, version=2021, nutscode='ITF11')
print(mydf, max=50)

# Retrive data for years 2015 and 2018
mydf <- ardeco_get_dataset_data('SNPTD', year='2015,2018', version=2021, nutscode='ITF11')
print(mydf, max=50)

# Retrive data for years from 2015 to 2018
mydf <- ardeco_get_dataset_data('SNPTD', year='2015-2018', version=2021, nutscode='ITF11')
print(mydf, max=50)


## -----------------------------------------------------------------------------
# Check the available dimensions for variable SNPTN
ardeco_get_dataset_list('SNPTN')

# Retrive data only for sex = 'Males' and age = 'Total'
mydf <- ardeco_get_dataset_data('SNPTN', sex='Males', age = 'Total')
print(mydf, max=50)

## -----------------------------------------------------------------------------
# Check available tercet for variable SNPTD
ardeco_get_tercet_list('SNPTD')

# Retrieve absolute values for tercet 'Urban-Rural Typology' for variable 'SNPTD' for country 'IT', year 2020, nuts version 2021
mydf <- ardeco_get_dataset_data('SNPTD', tercet_code=1, nutscode='IT', year=2020, version=2021)
print(mydf)

# Retrieve share for tercet 'Urban-Rural Typology' for variable 'SNPTD' for country 'IT', year 2020, nuts version 2021
mydf <- ardeco_get_dataset_data('SNPTD', tercet_code=1, nutscode='IT', year=2020, version=2021, show_perc=TRUE)
print(mydf)


## -----------------------------------------------------------------------------
# Check available tercet classes for variable SNPTD
ardeco_get_tercet_list('SNPTD')

# Retrieve absolute value for tercet class 'Predominantly urban' for variable 'SNPTD' for country 'IT', year 2020, nuts version 2021
mydf <- ardeco_get_dataset_data('SNPTD', tercet_class_code=0, nutscode='IT', year=2020, version=2021)
print(mydf)

# Retrieve share for tercet class 'Predominantly urban' for variable 'SNPTD' for country 'IT', year 2020, nuts version 2021
mydf <- ardeco_get_dataset_data('SNPTD', tercet_class_code=0, nutscode='IT', year=2020, version=2021, show_perc=TRUE)
print(mydf)

