## Visualizing Global Map of cases confirmed, deaths and recovered across the globe ##

# libraries and setup
getwd()
setwd("/Users/paulapivat/Desktop")
library(rgdal)    # download as geospatial, then change to data frame, then plot in ggplot2, is faster
library(broom)   # required to turn geospatial data into dataframe, so we can read into ggplot2 (this is called “fortify”) 


