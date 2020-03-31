### Shiny Dashboard ###

## directory 

## packages
library(tidyverse)
library(rsconnect)

## load files
full_data <- read_csv("/Users/paulapivat/Desktop/covid/dashboard/full_data.csv")

# sub dataframe Total Cases Thailand
thailand_full_data <- full_data %>% filter(location=="Thailand")

# add new columns
thailand_full_data[,"Changes"] <- NA 
thailand_full_data[,"Growth"] <- NA

# Changes in Totals (same as new_cases)
# subtract value from previous row

thailand_full_data <- thailand_full_data %>%
+ arrange(date) %>%
+ mutate(Changes = total_cases - lag(total_cases, default = first(total_cases)))

# Growth Factor
# divide value from previous row

thailand_full_data <- thailand_full_data %>%
+ arrange(date) %>%
+ mutate(Growth = Changes / lag(Changes, default = first(Changes)))

# convert Inf to NA - in Growth column
thailand_full_data$Growth = ifelse(thailand_full_data$Growth==Inf, NA, thailand_full_data$Growth)

# Growth RATE
# subtract previous row, then divide by previous row 

thailand_full_data <- thailand_full_data %>% 
+ arrange(date) %>% 
+ mutate(Growth_Rate = ((total_cases - lag(total_cases, default = first(total_cases))) / lag(total_cases, default = first(total_cases)))*100)

##### --- FLEXDASHBOARD, refer to sample.Rmd -------#######
## NOTE: Not everything here will stay on the dashboard
## The dashboard changes as new and better information is available

# Growth Rate (%) from yesterday
yesterday_growth_rate <- format(round(full_thailand$Growth_Rate[nrow(full_thailand)], 2), nsmall = 0)
valueBox(yesterday_growth_rate, color = "#333399")

# Today Growth Factor
today_growth <- format(round(full_thailand$Growth[nrow(full_thailand)], 2), nsmall = 2)
valueBox(today_growth, color = ifelse(today_growth=="NA", "#808080", ifelse(today_growth < 1.15, "#33cc33", "#FF3300")))

#valueBox(today_growth, color = ifelse(today_growth < 1, "#33cc33", "#ff3300"))

# Total COVID-19 tests performed
# data source: https://ddc.moph.go.th/viralpneumonia/eng/index.php
# in png file, need manual update
total_test <- 7566
valueBox(total_test, color = "#36454F")

# Yesterday New Cases
yesterday_new <- format(round(full_thailand$new_cases[nrow(full_thailand)-1], 2), nsmall = 0)
valueBox(yesterday_new, color = "#9fbfdf")

# Today New Cases
today_new <- format(round(full_thailand$new_cases[nrow(full_thailand)], 2), nsmall = 0)
valueBox(today_new, icon = "fa-pencil", color = "#336699")

# Total Cases
today_total <- format(round(full_thailand$total_cases[nrow(full_thailand)], 2), nsmall = 0)
valueBox(today_total, color = "#36454F")

## Plots

# New Cases Over Time
thai_new_bar <- ggplot(data = full_thailand, aes(x=date)) 
+ geom_bar(aes(y=new_cases), stat = "identity", size=2, fill="steelblue", color = "white", alpha = 0.8) 
+ labs(title = "New Cases", subtitle = "Jan 21 - present") 
+ theme_classic() 
+ theme(axis.text.x = element_blank())

# Total Cases Over Time
thai_total_bar <- ggplot(data = full_thailand, aes(x=date)) 
+ geom_bar(aes(y=total_cases), stat = "identity", size=1, fill="steelblue", color = "white", alpha = 0.8) 
+ labs(title = "Total Cases", subtitle = "Jan 21 - present") 
+ theme_classic() 
+ theme(axis.text.x = element_blank())

# Ratio Tests per positive case over time
test_per_case <- ggplot(data = ddc_who_data, mapping = aes(x = date, y = testpercase)) 
+ geom_bar(stat = "identity", size=2, fill = "#006666", color = "white", alpha = 0.8) 
+ theme_classic() 
+ labs(title="Ratio Test Per Positive Case", subtitle = "Jan 21 - present", y = "Test Per Case")


#### Data Frames Created ####
full_data 
thailand_full_data  # ECDC March 19th
who_data            # WHO March 17th 
thailand_who_data   # WHO March 17th 

ddc_who_data        # custom dataset
standard_data       # data frame used exclusively for data entry (since March 22)

## World Health Organization, WHO) ##
who_data <- read_csv("/Users/paulapivat/Desktop/covid/dashboard/who_full_data.csv")

## Manual process of reconciling WHO data with DDC data
ddc_who_data <- thailand_who_data
# March 14th update WHO total cases from 75 to 82; update new_cases from 0 to 7
ddc_who_data[54,5] <- 82
ddc_who_data[54,3] <- 7

# March 15th update WHO total cases from 75 to 114; update new_cases from 0 to 32
ddc_who_data[55,5] <- 114
ddc_who_data[55,3] <- 32

# March 16th update WHO total cases from 114 to 147; update new_cases from 39 to 33
ddc_who_data[56,5] <- 147
ddc_who_data[56,3] <- 33

# March 17th update WHO total cases from 147 to 177; 
ddc_who_data[57,5] <- 177
ddc_who_data[57,3] <- 30

# March 18th use Thai DDC total cases at 212
# backup
ddc_who_data2 <- ddc_who_data
ddc_who_data[58,] <- c('2020-03-18', "Thailand", 35, 0, 212, 1)

# March 19th use Thai DDC total cases at 272
ddc_who_data[59,] <- c('2020-03-19', "Thailand", 60, 0, 272, 1)

# March 20th use Thai DDC total cases at 322
ddc_who_data[60,] <- c('2020-03-20', "Thailand", 50, 0, 322, 1)

# Create new column Changes (daily new cases)

ddc_who_data[,"Changes"] <- NA 

# ***** ERROR ****** #
# possible that entry method of new rows causes coercion to "chr" type
# need to try specifying data type while adding new rows
# change all numbers to numeric (as in thailand_who_data)
ddc_who_data$new_cases <- as.numeric(ddc_who_data$new_cases)
ddc_who_data$new_deaths <- as.numeric(ddc_who_data$new_deaths)
ddc_who_data$total_cases <- as.numeric(ddc_who_data$total_cases)
ddc_who_data$total_deaths <- as.numeric(ddc_who_data$total_deaths)

# now this works without error
# calculate Changes (should be same as new_cases)
ddc_who_data <- ddc_who_data %>%
+ arrange(date) %>%
+ mutate(Changes = total_cases - lag(total_cases, default = first(total_cases)))


# Create new column Growth Factor (today new cases / yesterday new cases)

ddc_who_data[,"Growth_Factor"] <- NA

ddc_who_data <- ddc_who_data %>%
+ arrange(date) %>%
+ mutate(Growth_Factor = Changes / lag(Changes, default = first(Changes)))

ddc_who_data$Growth_Factor = ifelse(ddc_who_data$Growth_Factor==Inf, NA, ddc_who_data$Growth_Factor)


# Create new column for Growth Rate ((today total - yesterday total) / yesterday total)

ddc_who_data <- ddc_who_data %>% 
+ arrange(date) %>% 
+ mutate(Growth_Rate = ((total_cases - lag(total_cases, default = first(total_cases))) / lag(total_cases, default = first(total_cases)))*100)

# Write to csv
write.csv(ddc_who_data, "/Users/paulapivat/Desktop/covid/dashboard/ddc_who_data.csv")

# Deploy to Shiny

library(rsconnect)

# move sample.Rmd to same directory as ddc_who_data.csv
# CANNOT contain absolute paths: df_full <- read.csv("/Users/paulapivat/Desktop/covid/dashboard/ddc_who_data.csv")
# Error: Paths should be to files within the project directory.

# create director to /Desktop/covid/shiny
# place sample.Rmd and ddc_who_data.csv in the same directory
# within sample.Rmd: df_full <- read.csv("ddc_who_data.csv")  *relative, not absolute path

# NOTE: If you are going to deploy your app on a system other than yours 
# then you can't use absolute paths (to a local file), 
# you have to use file paths relative to the app's root folder.

# either Republish or Other Destination in RStudio


###########------- Daily Data Entry -------#########

## Protocol Update: March 22nd
# use standard_data exclusively for data entry
# when ready to publish, copy to ddc_who_data, 
# then re-create: Changes, Growth_Factor, Growth_Rate, 
# then re-create: Changes_PUI, Growth_Factor_PUI, Growth_Rate_PUI


# then enter new data
# use list to prevent changing data types
# change to standard_data
ddc_who_data[61,] <- c('2020-03-21', 'Thailand', 89, 0, 411, 1)
standard_data[62,] <- list('2020-03-22', 'Thailand', 188, 0, 599, 1, 10378)
standard_data[63,] <- list('2020-03-23', 'Thailand', 122, 0, 721, 1, 10955)
standard_data[64,] <- list('2020-03-24', 'Thailand', 106, 3, 827, 4, 11807)
standard_data[65,] <- list('2020-03-25', 'Thailand', 107, 0, 934, 4, 13027)
standard_data[66,] <- list('2020-03-26', 'Thailand', 111, 0, 1045, 4, 14122)
standard_data[67,] <- list('2020-03-27', 'Thailand', 91, 1, 1136, 5, 15342)
standard_data[68,] <- list('2020-03-28', 'Thailand', 109, 1, 1245, 6, 16531)
standard_data[69,] <- list('2020-03-29', 'Thailand', 143, 1, 1388, 7, 17175)
standard_data[70,] <- list('2020-03-30', 'Thailand', 136, 2, 1524, 9, 17829)
standard_data[71,] <- list('2020-03-31', 'Thailand', 127, 1, 1651, 10, 18776)


# Re-create Changes, Growth_Factor, Growth_Rate
ddc_who_data <- ddc_who_data %>% arrange(date) %>% mutate(Changes = total_cases - lag(total_cases, default = first(total_cases)))
ddc_who_data <- ddc_who_data %>% arrange(date) %>% mutate(Growth_Factor = Changes / lag(Changes, default = first(Changes)))
ddc_who_data$Growth_Factor = ifelse(ddc_who_data$Growth_Factor==Inf, NA, ddc_who_data$Growth_Factor)
ddc_who_data <- ddc_who_data %>% arrange(date) %>% mutate(Growth_Rate = ((total_cases - lag(total_cases, default = first(total_cases))) / lag(total_cases, default = first(total_cases)))*100)

# Re-create Changes_PUI, Growth_Factor_PUI, Growth_Rate_PUI
ddc_who_data <- ddc_who_data %>% arrange(date) %>% mutate(Changes_PUI = pui - lag(pui, default = first(pui)))
ddc_who_data <- ddc_who_data %>% arrange(date) %>% mutate(Growth_Factor_PUI = Changes_PUI / lag(Changes_PUI, default = first(Changes_PUI)))
ddc_who_data$Growth_Factor_PUI = ifelse(ddc_who_data$Growth_Factor_PUI==Inf, NA, ddc_who_data$Growth_Factor_PUI)
ddc_who_data <- ddc_who_data %>% arrange(date) %>% mutate(Growth_Rate_PUI = ((pui - lag(pui, default = first(pui))) / lag(pui, default = first(pui)))*100)

# testpercase
ddc_who_data <- ddc_who_data %>% mutate(testpercase = ddc_who_data$pui/ddc_who_data$total_cases)


# Write to csv to desktop
write.csv(ddc_who_data, "/Users/paulapivat/Desktop/ddc_who_data.csv")

# place ddc_who_data.csv file in same folder as sample.Rmd (replace previous one)
# Republish

###### ---- New Data: Patients Under Investigation (Testing) ---- ######

# create standard data set used only for data entry (no manipulations)
standard_data <- ddc_who_data
standard_data$Changes <- NULL
standard_data$Growth_Factor <- NULL
standard_data$Growth_Rate <- NULL

# create new column "PUI" (patients under investigation, aka tests performed)
standard_data[,"pui"] <- NA

# data source: Situation Reports, Thai DDC https://ddc.moph.go.th/viralpneumonia/eng/situation.php
# jan 21 - 31
standard_data$pui <- c(38, 46, 53, 60, 84, 102, 136, 158, 202, 280, 344)

# feb 1 - 29 (Feb 13*, 14*, 15*, )
standard_data$pui <- c(38, 46, 53, 60, 84, 102, 136, 158, 202, 280, 344, 
382, 485, 492, 549, 595, 615, 654, 679, 679, 702, 799, 823, 777, 804, 821, 837, 872, 957, 1052,
1151, 1252, 1355, 1453, 1580, 1798, 2064, 2437, 2798, 2953)

# mar 1 - 21 (Mar 14*, Mar 16*, Mar 20?, Mar 21)

standard_data$pui <- c(38, 46, 53, 60, 84, 102, 136, 158, 202, 280, 344, 
382, 485, 492, 549, 595, 615, 654, 679, 679, 702, 799, 823, 777, 804, 821, 837, 872, 957, 1052,
1151, 1252, 1355, 1453, 1580, 1798, 2064, 2437, 2798, 2953,
3252, 3519, 3680, 3895, 4023, 4234, 4366, 4518, 4682, 4848, 5232, 5496, 5713, 5713, 6545, 7045,
7045, 8157, 8729, 9670, 10343)

# correlation between new cases and new tests per day
library(moderndive)

# 0.352
get_correlation(data = ddc_who_data, formula = Changes_PUI ~ Changes, na.rm = TRUE)

# side-by-side bar chart
