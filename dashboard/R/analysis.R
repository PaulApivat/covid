### Shiny Dashboard ###

## directory 

## package
library(tidyverse)

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


#### Data Frames Created ####
full_data 
thailand_full_data  # ECDC March 19th
who_data            # WHO March 17th 
thailand_who_data   # WHO March 17th 

ddc_who_data        # custom dataset

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












