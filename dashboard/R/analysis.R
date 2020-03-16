### Shiny Dashboard ###

## directory 

## package
library(tidyverse)

## load files
new_cases <- read_csv("/Users/paulapivat/Desktop/covid/dashboard/new_cases.csv")
full_data <- read_csv("/Users/paulapivat/Desktop/covid/dashboard/full_data.csv")

## sub data frame Thailand
new_cases_thailand <- new_cases %>%
+ select(date, World, Thailand)

## add new columns
new_cases_thailand[,"World_Changes"] <- NA
new_cases_thailand[,"Thailand_Changes"] <- NA

## subtract value from previous row using dplyr
# World
new_cases_thailand <- new_cases_thailand %>%
+ arrange(date) %>%
+ mutate(World_Changes = World - lag(World, default = first(World)))

# Thailand
new_cases_thailand <- new_cases_thailand %>%
+ arrange(date) %>%
+ mutate(Thailand_Changes = Thailand - lag(Thailand, default = first(Thailand)))



