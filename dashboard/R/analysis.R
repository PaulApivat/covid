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



## Prepare to Plot

# convert Inf to NA - Thailand_Growth_Factor & World_Growth_Factor
new_cases_thailand$Thailand_Growth_Factor = ifelse(new_cases_thailand$Thailand_Growth_Factor==Inf, NA, new_cases_thailand$Thailand_Growth_Factor)

new_cases_thailand$World_Growth_Factor = ifelse(new_cases_thailand$World_Growth_Factor==Inf, NA, new_cases_thailand$World_Growth_Factor)

# ACCESS LAST ROW in Thailand_Growth_Factor column
new_cases_thailand$Thailand_Growth_Factor[nrow(new_cases_thailand)]


# basic bar plot of Thailand's Growth Factor
ggplot(data = new_cases_thailand, mapping = aes(x = date, y = Thailand_Growth_Factor)) + geom_bar(stat = "identity") + ylim(-1,2)
# basic bar plot of World's Growth Factor
ggplot(data = new_cases_thailand, mapping = aes(x = date, y = World_Growth_Factor)) + geom_bar(stat = "identity") + ylim(-1,2)

# plot bar chart overlay line graph
# Thailand New Cases and Growth Factor

ggplot(data = new_cases_thailand, aes(x=date)) 
+ geom_bar(aes(y=Thailand), stat = "identity", size=1, fill = "#69b3a2", color = "white", alpha=0.4) 
+ geom_line(aes(y=Thailand_Growth_Factor), size=2, color = "red") 
+ theme_classic() 
+ ggtitle("New Cases vs Growth Factor")

# add total cases column for Thailand
thailand_full_data <- full_data %>% filter(location=="Thailand")
new_cases_thailand[,'Thailand_Total_Cases'] <- thailand_full_data$total_cases

# plot bar chart overlay line graph
# Thailand Total Cases and Growth Factor

ggplot(data = new_cases_thailand, aes(x=date)) 
+ geom_bar(aes(y=Thailand_Total_Cases), stat = "identity", size=1, fill = "steelblue", color = "white", alpha = 0.4) 
+ geom_line(aes(y=Thailand_Growth_Factor), size=2, color = "red") 
+ theme_classic() 
+ ggtitle("Total Cases vs Growth Factor")

#### Italy ####

# create data frame
italy_full_data <- full_data %>% filter(location=="Italy")

# create Change in new cases
italy_full_data <- italy_full_data %>%
+ arrange(date) %>%
+ mutate(change_new_cases = new_cases - lag(new_cases, default = first(new_cases)))

# create Growth Factor in new cases
italy_full_data <- italy_full_data %>%
+ arrange(date) %>%
+ mutate(growth_new_cases = change_new_cases / lag(change_new_cases, default = first(change_new_cases)))

# change Inf to NA
italy_full_data$growth_new_cases = ifelse(italy_full_data$growth_new_cases==Inf, NA, italy_full_data$growth_new_cases)

# plots
# basic bar plot of Italy's Growth Factor
ggplot(data = italy_full_data, aes(x=date)) 
+ geom_bar(aes(y=growth_new_cases), stat = "identity", size=1, fill = "steelblue", color = "white", alpha = 0.4)

#### Scatter Plot with Best Fitting Line (Linear Regression) ####

# Italy - Total Cases
ggplot(data = italy_full_data, mapping = aes(x=date, y=total_cases)) 
+ geom_point() 
# transfor Italy's total cases to a logarithmic transformation
+ scale_y_continuous(trans = "log10") 
# linear regression line through the points (after log transformation)
+ geom_smooth(method = "lm")
+ labs(title = "Italy Total Cases with Logarithmic transformation", x = "Date", y = "Number of Cases")

# note: (rough estimate) going off regression line, Italy's total cases, on average, will
# 100x every 16 days
# this is only after we've seen exponential growth
# how can we tell people the growth rate in real-time?
# how to interpret negative growth

# Italy - Change in New Cases
# Note: recommended 
# roughly 10x new cases in 10 days
ggplot(data = italy_full_data, aes(x=date, y=change_new_cases)) 
+ geom_point() 
+ scale_y_continuous(trans = "log10") 
+ geom_smooth(method = "lm", se = FALSE) 
+ labs(title = "Italy: Changes in new cases with Logarithmic transformation", x = "Date", y = "Changes in New Cases")

#### Data Frames Created ####
new_cases 
new_cases_thailand
full_data 
italy_full_data
thailand_full_data

















