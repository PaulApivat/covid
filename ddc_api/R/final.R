# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

# load data
load(file = 'ddc_api.RData')

# packages
library(httr)
library(jsonlite)
library(tidyverse)
library(geofacet)
library(ggrepel)
library(reshape2)  # dependency in create_new_grid()

# GET() request from httr package
cases = GET('https://covid19.th-stat.com/api/open/cases')

# check for status_code 200 (fetch success)
View(cases)

# Convert raw Unicode into character vector (note: vector of length [1])
# note this is JSON in character format (still not usable)
rawToChar(cases$content)

# From Character Vector, convert to list data structure using fromJSON() in jsonlite library
cases_data = fromJSON(rawToChar(cases$content))

# for timeline_data, cases_data, we're interested in $Data dataframe
# This is dataframe format ready for further analysis in R
# Note: still need to View()
# DATE: last API update May 23, 2020
View(cases_data$Data)

###### data from to do tile map #######
cases_data$Data
#######################################
df <- cases_data$Data



