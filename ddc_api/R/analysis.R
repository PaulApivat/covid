# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

##### INTERACT WITH DDC API ######

## NOTE: below is simple API request; some APIs require user-info

# packages and libraries
install.packages(c('httr', 'jsonlite', 'tidyverse'))

library(httr)
library(jsonlite)
library(tidyverse)

# GET() request from httr package
timeline = GET('https://covid19.th-stat.com/api/open/timeline')
today = GET('https://covid19.th-stat.com/api/open/today')
cases = GET('https://covid19.th-stat.com/api/open/cases')

# Check for 'Status: 200' success
# Also View(timeline), View(today) to get feel for data structure (e.g., url, content, date etc)

# Response [https://covid19.th-stat.com/api/open/timeline]
  # Date: 2020-05-04 07:49
  # Status: 200
  # Content-Type: application/json
  # Size: 19.4 kB

# Convert raw Unicode into character vector (note: vector of length [1])
# note this is JSON in character format (still not usable)
rawToChar(timeline$content)
rawToChar(today$content)
rawToChar(cases$content)

# From Character Vector, convert to list data structure using fromJSON() in jsonlite library
timeline_data = fromJSON(rawToChar(timeline$content))
today_data = fromJSON(rawToChar(today$content))
cases_data = fromJSON(rawToChar(cases$content))


# for timeline_data, cases_data, we're interested in $Data dataframe
View(timeline_data$Data)
View(cases_data$Data)


