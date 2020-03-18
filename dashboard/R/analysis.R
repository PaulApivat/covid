### Shiny Dashboard ###

## directory 

## package
library(tidyverse)

## load files
new_cases <- read_csv("/Users/paulapivat/Desktop/covid/dashboard/new_cases.csv")
full_data <- read_csv("/Users/paulapivat/Desktop/covid/dashboard/full_data.csv")

# sub dataframe Total Cases Thailand
thailand_full_data <- full_data %>% filter(location=="Thailand")

# add new columns
thailand_full_data[,"Changes"] <- NA 
thailand_full_data[,"Growth"] <- NA

# Changes in Totals
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
valueBox(today_growth, color = ifelse(today_growth < 1, "#33cc33", "#ff3300"))

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
new_cases 
full_data 
thailand_full_data

















