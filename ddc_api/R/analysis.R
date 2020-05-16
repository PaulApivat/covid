# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

# load RData
load(file = 'ddc_api.RData')

##### INTERACT WITH DDC API ######

## NOTE: below is simple API request; some APIs require user-info - password, username

# packages and libraries
install.packages(c('httr', 'jsonlite', 'tidyverse'))

library(httr)
library(jsonlite)
library(tidyverse)

# GET() request from httr package
timeline = GET('https://covid19.th-stat.com/api/open/timeline')
today = GET('https://covid19.th-stat.com/api/open/today')
cases = GET('https://covid19.th-stat.com/api/open/cases')
cases_sum = GET('https://covid19.th-stat.com/api/open/cases/sum')

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
rawToChar(cases_sum$content)

# From Character Vector, convert to list data structure using fromJSON() in jsonlite library
timeline_data = fromJSON(rawToChar(timeline$content))
today_data = fromJSON(rawToChar(today$content))
cases_data = fromJSON(rawToChar(cases$content))
cases_sum_data = fromJSON(rawToChar(cases_sum$content))

# for timeline_data, cases_data, we're interested in $Data dataframe
# This is dataframe format ready for further analysis in R
# Note: still need to View()
View(timeline_data$Data)
View(cases_data$Data)

View(cases_sum_data$Province)

## NOTE cases_sum_data is nested list, so conversion to data.frame requires inspection of several lists.
View(data.frame(Reduce(rbind, cases_sum_data)))
View(data.frame(Reduce(rbind, cases_sum_data$Province)))

###### data from to do tile map #######
cases_data$Data
#######################################
df <- cases_data$Data
df2 <- timeline_data$Data

######## EDA ########
# note: see geofacet.R file
# using mygrid3_gender, mygrid3_cases
# using covidthai2 dataframe (same as df, but with provincial code)

### visualizing outliers ###

# cases (mygrid3_cases)
cases_outlier <- ggplot(data = mygrid3_cases, mapping = aes(x = reorder(ProvinceEn, sum_cases), y = sum_cases)) 
  + geom_bar(stat = 'identity') 
  + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
  + labs(x = 'Provinces', y = 'Total Cases', title = 'Thailand: Total Covid-19 Cases Jan-May')

# cases by gender (mygrid3_gender)
cases_outlier_gender <- ggplot(data = mygrid3_gender, mapping = aes(x = reorder(ProvinceEn, cases), y=cases, fill=GenderEn)) 
  + geom_bar(stat = 'identity') 
  + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black')) 
  + labs(x = 'Provinces', y = 'Cases by Gender', title = 'Thailand: Total Covid-19 Cases by Gender, Jan-May')

# cases by Nationality
covidthai2 %>% group_by(ProvinceEn, Nation, code) %>% tally(sort = TRUE) -> province_by_nationality
colnames(province_by_nationality)[4] <- 'cases'

mygrid3_nation <- province_by_nationality %>%
  inner_join(mygrid3, by = 'code')

# plot by Nationality (Thai = Outlier)
# note: Thai character does not render (see ShowText below)
ggplot(data = mygrid3_nation, mapping = aes(x=reorder(Nation, cases), y=cases)) 
  + geom_bar(stat = 'identity') 
  + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))

#### ShowText
# source: https://github.com/yixuan/showtext
# Google Fonts: Krub by Cadson Demak 
# source: https://fonts.google.com/specimen/Krub?query=kru
# manually add Krub to Library/Fonts path (macOS)
library(showtext)
View(font_files())  # should see newly added 'Krub' family font
font_add('Krub', 'Krub-Regular.ttf', italic = 'Krub-Italic.ttf')   #sysfonts part of showtext library

# Now Thai font renders
ggplot(data = mygrid3_nation, mapping = aes(x=reorder(Nation, cases), y=cases)) 
+ geom_bar(stat = 'identity') 
# have x-axis text label render in 'Krub' family
+ theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black', family = 'Krub'))
# show foreign font on the plot itself 
+ annotate('text', 10, 100, family = 'Krub', size = 50, label = 'ไทย')

### To show 'Thai' nationality as outlier
# create factor of Thai & Foreign
# disproportionately more Thai - across all province? (or mostly Bangkok)
mygrid3_nation[,'nation_fct'] <- NA
mygrid3_nation$nation_fct <- ifelse(mygrid3_nation$Nation=='ไทย', 'Thai', 'Foreign')

# order by ProvinceEn (re-order doesn't work)
ggplot(data = mygrid3_nation, mapping = aes(x=reorder(ProvinceEn, cases), y=cases, fill=nation_fct)) 
  + geom_bar(stat = 'identity') 
  + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))

# re-order works now
mygrid3_nation %>% 
  # if want to fill = nation_fct need to add in group_by first
  group_by(ProvinceEn, nation_fct) %>% 
  # to get re-order by cases to work, need to sum_cases_province first
  # originally, cases were by Nationality (not province) so re-order doesn't work
  summarize(sum_cases_province = sum(cases)) %>% 
  ggplot(aes(x=reorder(ProvinceEn, sum_cases_province), y=sum_cases_province, fill = nation_fct)) 
  + geom_bar(stat = 'identity') 
  + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))



# cases by age
# select out ProvinceEn, code, Age, then figure out cases later
covidthai2 %>%
+ select(Age, ProvinceEn, code) -> province_by_age

mygrid3_age <- province_by_age %>%
+ inner_join(mygrid3, by = 'code')

# summary(mygrid3_age$Age)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0.00   27.00   37.00   38.82   49.00   97.00 

mygrid3_age[,'age_fct'] <- NA

mygrid3_age$age_fct <- ifelse((mygrid3_age$Age > 0 & mygrid3_age$Age < 11), '1-10', mygrid3_age$age_fct)
mygrid3_age$age_fct <- ifelse((mygrid3_age$Age > 10 & mygrid3_age$Age < 21), '11-20', mygrid3_age$age_fct)
mygrid3_age$age_fct <- ifelse((mygrid3_age$Age > 20 & mygrid3_age$Age < 31), '21-30', mygrid3_age$age_fct)
mygrid3_age$age_fct <- ifelse((mygrid3_age$Age > 30 & mygrid3_age$Age < 41), '31-40', mygrid3_age$age_fct)
mygrid3_age$age_fct <- ifelse((mygrid3_age$Age > 40 & mygrid3_age$Age < 51), '41-50', mygrid3_age$age_fct)
mygrid3_age$age_fct <- ifelse((mygrid3_age$Age > 50 & mygrid3_age$Age < 61), '51-60', mygrid3_age$age_fct)
mygrid3_age$age_fct <- ifelse((mygrid3_age$Age > 60 & mygrid3_age$Age < 71), '61-70', mygrid3_age$age_fct)
mygrid3_age$age_fct <- ifelse((mygrid3_age$Age > 70), '70 and above', mygrid3_age$age_fct)


mygrid3_age %>% group_by(ProvinceEn, code, age_fct, row, col) %>% tally(sort = TRUE) -> mygrid3_age_fct
colnames(mygrid3_age_fct)[6] <- 'cases'


### Outliers by Age (Factor)
mygrid3_age_fct %>%
  group_by(ProvinceEn, age_fct) %>%
  summarize(sum_cases_province = sum(cases)) %>%
  ggplot(aes(x=reorder(ProvinceEn, sum_cases_province), y=sum_cases_province, fill=age_fct)) 
  + geom_bar(stat = 'identity') 
  + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


# Districts of Bangkok
# NOTE: mostly missing data (n = 847)
View(covidthai2 %>% 
  filter(ProvinceEn=='Bangkok') %>% 
  group_by(District) %>% tally(sort = TRUE))

