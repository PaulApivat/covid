# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

# load data
load(file = 'ddc_api.RData')

# packages
library(httr)
library(jsonlite)
library(tidyverse)
library(geofacet)
library(ggrepel)
library(reshape2)  # dependency in create_new_grid()

# GET() request from httr package
cases = GET('https://covid19.th-stat.com/api/open/cases')

# check for status_code 200 (fetch success)
View(cases)

# Convert raw Unicode into character vector (note: vector of length [1])
# note this is JSON in character format (still not usable)
rawToChar(cases$content)

# From Character Vector, convert to list data structure using fromJSON() in jsonlite library
cases_data = fromJSON(rawToChar(cases$content))

# for timeline_data, cases_data, we're interested in $Data dataframe
# This is dataframe format ready for further analysis in R
# Note: still need to View()
# DATE: last API update May 23, 2020
View(cases_data$Data)

###### data from to do tile map #######
cases_data$Data
#######################################
df <- cases_data$Data

## Find sum of cases
View(df %>% group_by(ProvinceEn) %>% tally(sort = TRUE))
df %>% group_by(ProvinceEn) %>% tally(sort = TRUE) -> df_sum

##### See geofacet.R for process on making mygrid3 #####
######### Apply Function create_new_grid #########
## source: https://github.com/johnjosephhorton/geofacet/blob/master/R/create_new_grid.R
## use create_new_grid() function to grid a reduced grid
reduce_grid <- create_new_grid(x,y,province, num.iterations = 500)

## minor

########## Create new Grid with Geo Grid Designer ############

# step 2: (use 19 instead of 18 because all rows, col need to be positive number for Geo Grid Designer)
reduce_grid4$row <- 19 - reduce_grid4$row
# step 3: put this output in Geo Grid Designer
cat(format_csv(reduce_grid4))


# Write data frame to a delimited file (Geo Grid Designer accepts this format)

mygrid3 <- data.frame(
  row = c(2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 10, 10, 10, 11, 11, 12, 13, 14, 15, 16, 17, 17, 17, 18, 19, 20, 20, 20, 21, 21),
  col = c(3, 2, 3, 4, 1, 9, 8, 2, 5, 3, 2, 10, 9, 8, 7, 6, 5, 4, 9, 2, 8, 3, 4, 6, 7, 5, 10, 11, 3, 9, 8, 6, 7, 4, 5, 10, 8, 4, 3, 6, 7, 9, 5, 9, 3, 2, 5, 7, 8, 4, 6, 3, 4, 7, 8, 5, 6, 4, 7, 8, 3, 9, 3, 3, 3, 3, 3, 4, 5, 3, 5, 5, 7, 5, 6, 8, 7),
  name = c("Chiang Rai", "Chiang Mai", "Phayao", "Nan", "Mae Hong Son", "Bungkan", "Nong Khai", "Lampang", "Uttaradit", "Phrae", "Lamphun", "Nakhon Phanom", "Sakon Nakhon", "Udon Thani", "Nong Bua Lam Phu", "Loei", "Phitsanulok", "Sukhothai", "Mukdahan", "Tak", "Kalasin", "Kamphaeng Phet", "Phichit", "Chaiyaphum", "Khon Kaen", "Phetchabun", "Amnat Charoen", "Ubon Ratchathani", "Nakhon Sawan", "Roi Et", "Maha Sarakham", "Saraburi", "Lop Buri", "Sing Buri", "Ang Thong", "Yasothon", "Si Sa Ket", "Chai Nat", "Uthai Thani", "Pathum Thani", "Nakhon Ratchasima", "Buri Ram", "Nonthaburi", "Surin", "Suphan Buri", "Kanchanaburi", "Bangkok", "Nakhon Nayok", "Prachin Buri", "Phra Nakhon Si Ayutthaya", "Samut Prakan", "Ratchaburi", "Nakhon Pathom", "Chachoengsao", "Sa Kaeo", "Samut Sakhon", "Chon Buri", "Samut Songkhram", "Rayong", "Chanthaburi", "Phetchaburi", "Trat", "Prachuap Khiri Khan", "Chumphon", "Ranong", "Surat Thani", "Phangnga", "Krabi", "Nakhon Si Thammarat", "Phuket", "Trang", "Phattalung", "Pattani", "Satun", "Songkhla", "Narathiwat", "Yala"),
  code = c("CRI", "CMI", "PYO", "NAN", "MSN", "BKN", "NKI", "LPG", "UTD", "PRE", "LPN", "NPM", "SNK", "UDN", "NBP", "LEI", "PLK", "STI", "MDH", "TAK", "KSN", "KPT", "PCT", "CPM", "KKN", "PNB", "ACR", "UBN", "NSN", "RET", "MKM", "SRI", "LRI", "SBR", "ATG", "YST", "SSK", "CNT", "UTI", "PTE", "NMA", "BRM", "NBI", "SRN", "SPB", "KRI", "BKK", "NYK", "PRI", "AYA", "SPK", "RBR", "NPT", "CCO", "SKW", "SKN", "CBI", "SKM", "RYG", "CTI", "PBI", "TRT", "PKN", "CPN", "RNG", "SNI", "PNA", "KBI", "NRT", "PKT", "TRG", "PLG", "PTN", "STN", "SKA", "NWT", "YLA"),
  stringsAsFactors = FALSE
)
geofacet::grid_preview(mygrid3)


# Prepare to join df_sum with mygrid3
colnames(df_sum)[1] <- 'name'
colnames(df_sum)[2] <- 'sum_cases'

# join table df_sum with mygrid3
mygrid3_cases_final <- mygrid3 %>%
    inner_join(df_sum, by = 'name')

# join mygrid3_cases_final with region_clusters
colnames(region_clusters)[1] <- 'name'

mygrid3_cases_final <- mygrid3_cases_final %>%
    inner_join(region_clusters, by = 'name')

## Figure out regional-averages to add baseline
mygrid3_cases_final %>% 
    group_by(Region) %>% 
    summarize(region_case_avg = mean(sum_cases))

# need to add regional_avg into the mygrid3_cases_alt data frame
mygrid3_cases_final[,'region_avg'] <- NA

# conditionally fill in region_avg column based on Region column
mygrid3_cases_final$region_avg <- ifelse(mygrid3_cases_final$Region=='Northern Region', 6.79, mygrid3_cases_final$region_avg)
mygrid3_cases_final$region_avg <- ifelse(mygrid3_cases_final$Region=='Central Region', 8.33, mygrid3_cases_final$region_avg)
mygrid3_cases_final$region_avg <- ifelse(mygrid3_cases_final$Region=='Northeastern', 5.38, mygrid3_cases_final$region_avg)
mygrid3_cases_final$region_avg <- ifelse(mygrid3_cases_final$Region=='Southern', 64.9, mygrid3_cases_final$region_avg)
mygrid3_cases_final$region_avg <- ifelse(mygrid3_cases_final$Region=='Greater Bangkok', 466, mygrid3_cases_final$region_avg)


# g2 base background
# note: scale_color_manual()
levels(mygrid3_cases_final$Region)

g2 <- ggplot(data = mygrid3_cases_final, 
        mapping = aes(x=reorder(Region, sum_cases), 
        y=sum_cases, 
        color=Region)) 
    + coord_flip() 
    + theme(legend.position = 'none', 
        panel.background = element_rect(color = 'white', 
        fill = 'white')) 
    + labs(x = NULL, y = 'Cases')
    + scale_color_manual(values = c('#ff7f00', '#e41a1c', '#377eb8', '#4daf4a', '#984ea3'))

# example plots with g2 base
g2 + geom_boxplot()
g2 + geom_line(size = 1)
g2 + geom_point(size = 1)
g2 + geom_point(size = 3, alpha = 0.15)

# visually add Regional Average
# note use set.seed(123) to keep points fixed
set.seed(123)
g2 
    + geom_jitter(size = 2, alpha = 0.4, width = 0.4) 
    + stat_summary(fun.y = mean, geom = 'point', size = 5)

# relate all points to a baseline; country average
country_avg_final <- mygrid3_cases_final %>% 
    summarize(case_avg = mean(sum_cases)) %>% 
    pull(case_avg)

set.seed(123)
g2 
    + geom_hline(aes(yintercept = country_avg_final), 
        color = "gray70", size = 0.6) 
    + geom_jitter(size = 2, alpha = 0.4, width = 0.4) 
    + stat_summary(fun.y = mean, geom = 'point', size = 5)

# add geom_segment from baseline to region-averages
# use region_avg column
set.seed(123)
g2 
    + geom_hline(aes(yintercept = country_avg_final), 
        color = "gray70", size = 0.6) 
    + geom_jitter(size = 2, alpha = 0.4, width = 0.4) 
    + stat_summary(fun.y = mean, geom = 'point', size = 5) 
    + geom_segment(aes(x = reorder(Region, sum_cases), 
        xend = Region, y = country_avg_final, 
        yend = mygrid3_cases_final$region_avg), 
        size = 0.8)



