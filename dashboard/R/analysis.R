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

## Change in new cases
## subtract value from previous row using dplyr

# World
new_cases_thailand <- new_cases_thailand %>%
+ arrange(date) %>%
+ mutate(World_Changes = World - lag(World, default = first(World)))

# Thailand
new_cases_thailand <- new_cases_thailand %>%
+ arrange(date) %>%
+ mutate(Thailand_Changes = Thailand - lag(Thailand, default = first(Thailand)))

## Growth factor of new cases

# World
new_cases_thailand <- new_cases_thailand %>%
+ arrange(date) %>%
+ mutate(World_Growth_Factor = World_Changes / lag(World_Changes, default = first(World_Changes)))

# Thailand
new_cases_thailand <- new_cases_thailand %>%
+ arrange(date) %>%
+ mutate(Thailand_Growth_Factor = Thailand_Changes / lag(Thailand_Changes, default = first(Thailand_Changes)))

## Prepare to Plot

# convert Inf to NA - Thailand_Growth_Factor & World_Growth_Factor
new_cases_thailand$Thailand_Growth_Factor = ifelse(new_cases_thailand$Thailand_Growth_Factor==Inf, NA, new_cases_thailand$Thailand_Growth_Factor)

new_cases_thailand$World_Growth_Factor = ifelse(new_cases_thailand$World_Growth_Factor==Inf, NA, new_cases_thailand$World_Growth_Factor)

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

# Italy
ggplot(data = italy_full_data, mapping = aes(x=date, y=total_cases)) 
+ geom_point() 
# transfor Italy's total cases to a logarithmic transformation
+ scale_y_continuous(trans = "log10") 
# linear regression line through the points (after log transformation)
+ geom_smooth(method = "lm")
+ labs(title = "Italy Total Cases with Logarithmic transformation", x = "Date", y = "Number of Cases")

# note: (rough estimate) going off regression line, Italy's cases, on average, will
# 100x every 16 days












