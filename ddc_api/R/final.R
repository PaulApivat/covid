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
library(patchwork)


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

# add annotation
set.seed(123)
g2_text <- g2 
    + geom_hline(aes(yintercept = country_avg_final), 
        color = "gray70", size = 0.6) 
    + geom_jitter(size = 2, alpha = 0.4, width = 0.4) 
    + stat_summary(fun.y = mean, geom = 'point', size = 5) 
    + geom_segment(aes(x = reorder(Region, sum_cases), 
        xend = Region, y = country_avg_final, 
        yend = mygrid3_cases_final$region_avg), 
        size = 0.8) 
    + annotate("text", x = 5.5, y = 250, size = 2.7, color = "gray20", label = "Country-wide average: 48.07 cases") 
    + annotate("text", x = 4.50, y = 1400, size = 2.7, color = "gray20", label = paste0('Bangkok is a huge outlier, \n with the most cases by far')) 
    + annotate('text', x = 3.8, y = 225, size = 2.7, color = "gray20", label = 'Regional average')  
    + annotate('text', x = 2.5, y = 200, size = 2.7, color = 'gray20', label = "Provinces per region")

# add arrows
set.seed(123)
g2_arrow <- g2_text 
    + geom_curve(data = arrows_final, 
        aes(x = x1, y = y1, xend = x2, yend = y2), 
        arrow = arrow(length = unit(0.07, 'inch')), 
        size = 0.4, color = 'gray20', curvature = -0.3)

# add capition: finalize
set.seed(123)
g2_final <- g2_arrow 
    + labs(caption = "Visualization: @paulapivat | Data: Department of Disease Control Open API", 
        y = "Total Cases", 
        title = "Covid-19 Cases in Thailand", 
        subtitle = "Jan 12 - May 23") 
    + theme(plot.caption = element_text(size = 9, color = "gray50"))

# create thai_province_region
thai_province_region_final <- ggplot(mygrid3_cases_final, 
        aes(xmin = col, 
            ymin = row, 
            xmax = col + 1, 
            ymax = row + 1, 
            fill = Region)) 
    + geom_rect(color = '#ffffff') 
    + theme_minimal() 
    + theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank()) 
    + geom_text(aes(x = col, y = row, label = code), 
        color = 'black', 
        alpha = 0.8, 
        nudge_x = 0.5, 
        nudge_y = -0.5, 
        size = 3) 
    + scale_y_reverse() 
    + theme(legend.position = 'none') 
    + scale_fill_manual(values = c('#ff7f00', '#e41a1c', '#377eb8', '#4daf4a', '#984ea3'))

# overlap grid map onto plot using annotation_custom()
g2_finalize <- g2_final 
    + annotation_custom(ggplotGrob(thai_province_region_final), 
        xmin = 0.5, 
        xmax = 4.5, 
        ymin = 600, 
        ymax = 1300)

# large version of thai_grid_map_g2
thai_grid_map_g2 <- mygrid3_cases_final %>% 
    filter(code != 'BKK') %>% 
    ggplot(aes(xmin = col, ymin = row, xmax = col + 1, ymax = row + 1, fill = sum_cases)) 
    + geom_rect(color = '#ffffff') 
    + theme_minimal() 
    + theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank()) 
    + geom_text(aes(x = col, y = row, label = code), 
        color = 'black', 
        alpha = 0.8, 
        nudge_x = 0.5, 
        nudge_y = -0.5, 
        size = 3) 
    + scale_y_reverse() 
    + scale_fill_gradient2(low = '#ffeda0', 
        mid = '#feb24c', 
        high = '#f03b20', 
        midpoint = 100, 
        na.value = 'white', 
        guide = 'colourbar', 
        aesthetics = 'fill') 
    + labs(fill = 'Cases')

# patchwork
library(patchwork)
g2_finalize + thai_grid_map_g2

#####################--------------- BANGKOK FOCUS------------ ##########################

## subset df dataframe into only bkk
df %>% filter(ProvinceEn=='Bangkok') -> covidthai3_bkk

## innerjoin covidthai3_bkk with mybkkgrid3 
# to get District names in English and Code

# NOTE: covidthai3_bkk originally had 1,551 rows, then shrunk to 621 after join
# because it had several entries that were NOT actual District and many blank entries
# joining with mybkkgrid3, which only had 50 rows, forced covidthai3_bkk to only have 
# District(s) that mybkkgrid3 had, lowering the number of rows from 1551 to 621

covidthai3_bkk <- covidthai3_bkk %>%
+ inner_join(mybkkgrid3, by = 'District')

# create sum_cases from covidthai3_bkk
# to first visualize Bangkok district from 'total cases' perspective

covidthai3_bkk %>% 
    group_by(name, code) %>% 
    tally(sort = TRUE) -> bkkdist_total_cases

colnames(bkkdist_total_cases)[1] <- 'DistrictEn'
colnames(bkkdist_total_cases)[3] <- 'sum_cases'

# join bkkdist_total_cases with mbkkgrid2a to get rol and col 
# to visualize sum_cases in regular tile map first

mybkkgrid2a_cases <- bkkdist_total_cases %>%
+ inner_join(mybkkgrid2a, by = 'code')

### CAVEAT MISSING DATA 
### 818 cases with NULL entry in 'District' field
View(df %>% filter(ProvinceEn=='Bangkok') %>% group_by(District) %>% tally(sort = TRUE))

## Final Tile Map BKK 50 Districts
bkkdist_cases <- ggplot(data = mybkkgrid2a_cases, mapping = aes(xmin = col, ymin = row, xmax = col + 1, ymax = row + 1, fill = sum_cases)) 
    + geom_rect(color = '#ffffff') 
    + theme_minimal() 
    + theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank()) 
    + geom_text(aes(x = col, y = row, label = name, family = 'Krub'), 
        color = 'black', 
        alpha = 0.8, 
        nudge_x = 0.5, 
        nudge_y = -0.5, 
        size = 3) 
    + scale_y_reverse() 
    + scale_fill_gradient2(low = '#edf8b1', mid = '#7fcdbb', high = '#2c7fb8', midpoint = 20, na.value = 'white', guide = 'colourbar', aesthetics = 'fill') 
    + labs(fill = 'Cases', 
        title = 'Covid-19 Cases in 50 Bangkok Districts: Jan 12 - May 23', 
        subtitle = 'Missing Data: n = 818', 
        caption = 'Visualization: @paulapivat | Data: Department of Disease Control Open API')

#### GEOFACET MAP BKK DISTRICT BY GENDER ####

# subset to contain district, code, gender and cases
covidthai3_bkk %>% 
    group_by(name, code, District, GenderEn) %>% 
    tally(sort = TRUE) -> bkkdist_by_gender2

# join with mybkkgrid2a to get row and col data
mybkkgrid2a_gender2 <- bkkdist_by_gender2 %>% 
    inner_join(mybkkgrid2a, by = 'code')

colnames(mybkkgrid2a_gender2)[1] <- 'DistrictEn'
colnames(mybkkgrid2a_gender2)[5] <- 'cases'

## using mybkkgrid2 as geo facet to allow english spelling of district names
mybkkgrid2[,5] <- NULL
colnames(mybkkgrid2)[3] <- 'name'

## Final Geofacet BKK District Gender breakdown
## note: Long names don't get displayed
## alternative: to use District Code - but most people won't know those letters.
# note: stretch out plot before downloading as png to see all district names

bkkdist_gender <- ggplot(data = mybkkgrid2a_gender2, mapping = aes(x=GenderEn, y=cases, fill=GenderEn)) 
    + geom_bar(position = 'dodge', stat = 'identity') 
    + facet_geo(~ code, grid = mybkkgrid2, label = 'name', scales = 'free_y') 
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black', size=6), 
        axis.text.y = element_text(size=6), 
        strip.text.x = element_text(size = 6, margin = margin(0.5, 2, 0.5, 2, 'mm')), 
        legend.position = 'none') 
    + scale_y_continuous(breaks = c(0, 5, 10, 25), labels = scales::number_format(accuracy = 1, decimal.mark = ',')) 
    + scale_fill_manual(values = c('#c51b7d', '#4d9221')) 
    + labs(x = '', y = '', 
        title = 'Covid-19 across 50 Bangkok Districts by Gender', 
        subtitle = 'Jan 12 - May 23, 2020', 
        caption = 'Visualization: @paulapivat | Data: Department of Disease Control Open API')

## Final Geo Grid Map for Thailand by Age Bracket
thai_grid_age <- mygrid3_age_fct %>% 
    filter(code != 'BKK') %>% 
    ggplot(aes(x=age_fct, y=cases, fill=age_fct)) 
    + geom_bar(position = 'dodge', stat = 'identity') 
    + geom_text(aes(label=cases), position = position_dodge(0.5), size = 2.5, vjust=1.25) 
    + facet_geo(~ code, grid = mygrid3, label = 'code', scales = 'free_y') 
    + theme(strip.text.x = element_text(size = 6, margin = margin(0.5, 2, 0.5, 2, 'mm')), 
        axis.text.y = element_blank(), 
        axis.text.x = element_blank()) 
    + labs(x = '', y = '', fill = 'Age Brackets', 
        title = 'Covid-19 across Thailand by Age, Jan 12 - May 23, 2020', 
        subtitle = 'Bangkok, as an outlier, is omitted', 
        caption = 'Visualization: @paulapivat | Data: Department of Disease Control Open API')



