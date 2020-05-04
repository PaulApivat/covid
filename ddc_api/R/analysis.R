# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

##### DDC API ######

# packages and libraries
install.packages(c('httr', 'jsonlite', 'tidyverse'))

library(httr)
library(jsonlite)
library(tidyverse)

# GET() request from httr package
timeline = GET('https://covid19.th-stat.com/api/open/timeline')
today = GET('https://covid19.th-stat.com/api/open/today')

# Check for 'Status: 200' success

# Response [https://covid19.th-stat.com/api/open/timeline]
  # Date: 2020-05-04 07:49
  # Status: 200
  # Content-Type: application/json
  # Size: 19.4 kB


