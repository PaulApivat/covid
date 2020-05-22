## steps

# load RData to load all global environment variables and functions created (create_new_grid())
load(file = 'ddc_api.RData')

## geofacet library allows to create custom grid for maps
# https://hafen.github.io/geofacet/
# facet_geo()

## create custom grid for Thailand

## enter --> grid_design(data = us_state_grid2, img = "http://bit.ly/us-grid")
## open application to see what csv row, col, code, name you need to create

## example data frame
mygrid <- data.frame(
  row = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 8),
  col = c(1, 12, 11, 10, 10, 11, 9, 7, 5, 2, 10, 3, 9, 11, 4, 1, 6, 11, 2, 6, 7, 5, 4, 8, 1, 9, 3, 10, 3, 4, 7, 10, 5, 1, 9, 6, 2, 8, 7, 2, 5, 1, 8, 6, 3, 4, 9, 9, 5, 4, 1),
  code = c("AK", "ME", "NH", "VT", "CT", "MA", "NY", "MI", "MN", "MT", "NJ", "ND", "PA", "RI", "SD", "WA", "WI", "DE", "ID", "IL", "IN", "IA", "NE", "OH", "OR", "VA", "WY", "DC", "CO", "KS", "KY", "MD", "MO", "NV", "NC", "TN", "UT", "WV", "AL", "AZ", "AR", "CA", "GA", "MS", "NM", "OK", "SC", "FL", "LA", "TX", "HI"),
  name = c("Alaska", "Maine", "New Hampshire", "Vermont", "Connecticut", "Massachusetts", "New York", "Michigan", "Minnesota", "Montana", "New Jersey", "North Dakota", "Pennsylvania", "Rhode Island", "South Dakota", "Washington", "Wisconsin", "Delaware", "Idaho", "Illinois", "Indiana", "Iowa", "Nebraska", "Ohio", "Oregon", "Virginia", "Wyoming", "District of Columbia", "Colorado", "Kansas", "Kentucky", "Maryland", "Missouri", "Nevada", "North Carolina", "Tennessee", "Utah", "West Virginia", "Alabama", "Arizona", "Arkansas", "California", "Georgia", "Mississippi", "New Mexico", "Oklahoma", "South Carolina", "Florida", "Louisiana", "Texas", "Hawaii"),
  stringsAsFactors = FALSE
)

## alternatively
## create grid using existing long & lat info
## https://john-joseph-horton.com/extending-geofacet-to-algorithmically-generate-grids-from-lat-long-data/

# step 1: import create_new_grid() function (save in .GlobalEnv - paste & enter)
# step 2: go thru usage of geofacet here: https://gist.github.com/johnjosephhorton/571e81fa81e20ed72d770a62cb4d2dcb#file-geofacet_usage_example-r
# load following libraries
library(geofacet)
library(tidyverse)
library(ggrepel)
library(reshape2)  # dependency in create_new_grid()

# step 3: create and save new grid


## open previous Thai map 
## find lat and long

# key is connecting lat/long to row/column to name of province/city

# see if you can get lat + long a country level (Thailand)
# see if you can get lat + long at city level (Bangkok) - need download from GADM.

################ Two data frames from 'thai_income.RData' in csv form ######
## df5b.csv
## thai1_tidy_join.csv

## id matched with province
df5b <- add_column(df5b, id = c("23", "53", "30", "31", "41", "12", "15", "60", "67", "57", 
"4", "49", "77", "69", "56", "44", "19", "51", "48", "8", "63", "20", "54", "55", "38", "46", 
"2", "13", "11", "73", "42", "26", "36", "3", "16", "24", "72", "7", "66", "62", "40", "39", 
"37", "22", "45", "65", "59", "70", "75", "76", "1", "34", "28", "9", "71", "14", "29", "17", 
"50", "6", "52", "21", "18", "25", "10", "33", "43", "64", "47", "5", "61", "58", "68", "35", 
"32", "74", "27"), .before = "Region_Province")

## join df5b and thai1_tidy by 'id' (confirmed 'id' = 77 province)
thai1_tidy_join <- thai1_tidy %>%
    inner_join(df5b, by = "id")

## thai1_tidy_join match 'id' and Region_Province
## thai1_tidy_join has long, lat, matched with id and Region_Province, but 605,879 rows


## need to see if write.csv() brings in ALL the data (worked)
thai_longlat_id <- read.csv('thai1_tidy_join.csv')
thai_province_id <- read.csv('df5b.csv')

##########################################################################

######### Apply Function create_new_grid #########
# source: https://github.com/johnjosephhorton/geofacet/blob/master/R/create_new_grid.R

#' Create an order-preserving, roughly square grid based on lat/long
#'
#' @param x a vector of unit longitudes 
#' @param y a vector of unit latitude 
#' @param unit.name a vector of names
#' @param num.iterations an integer of how many times to try shrinking
#' @importFrom magrittr "%>%"
#' @importFrom reshape2 melt
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @export

# save to global environment
create_new_grid <- function(x,y, unit.name, num.iterations = 100){
  num.units <- length(unit.name)
  index <- as.integer(1:num.units)
  # Order units both horizontally & verticaly 
  row <- rank(y)
  col <- rank(x)
  df <- data.frame(x,y,row,col,index, unit.name)

  # Populate position matrix 
  P = matrix(rep(0,num.units*num.units), nrow = num.units, ncol = num.units)
  for(i in 1:nrow(df)){
    r <- df[i,"row"]
    c <- df[i,"col"]
    P[r,c] <- df[i,"index"]
  }

  RowCollision <- function(r,P){
    "Returns true if by merging this row w/ one below generates a collision "
    top <- P[r,] > 0
    bot <- P[r + 1,] > 0
    as.integer(top %*% bot)
  }

  ColCollision <- function(c,P){
    "Returns true if by merging this col w/ one to right generates a collision "
    left <- P[,c] > 0
    right <- P[,c + 1] > 0
    as.integer(left %*% right)
  }

  CollapseRow <- function(r,P){
    "Returns a new matrix with collapsed row removed (shrinks matrix by 1 row)"
    top <- P[r,] 
    bot <- P[r + 1,]
    P2 <- P
    P2[r,] <- top + bot
    P2[-(r+1),]
  }

  CollapseCol <- function(col,P){
    "Returns a new matrix with collapsed col removed (shrinks matrix by 1 row)"
    left <- P[,col] 
    right <- P[,col + 1]
    P2 <- P
    P2[,col] <- left + right
    P2[,-(col+1)]
  }

  CollapsibleRows <- function(P){
    "Finds rows that could be collapsed"
    num.rows <- dim(P)[1]
    if(is.null(num.rows)) return(NULL)
    candidate.rows <- c()
    for (r in 1:(num.rows - 1)){
      if (RowCollision(r,P) == 0){
      candidate.rows <- c(candidate.rows, r)
      }
    }
    candidate.rows
  }

  CollapsibleCols <- function(P){
    "Finds columns that could be collapsed"
    num.cols <- dim(P)[2]
    if( is.null(num.cols) ) return(NULL)
    candidate.cols <- c()
    for (col in 1:(num.cols - 1)){
      if (ColCollision(col,P) == 0){
        candidate.cols <- c(candidate.cols, col)
      }
    }
    candidate.cols
  }

  ShrinkRow <- function(P){
    "Shrinks a row, if possible. If more than 1 row possible, selects at random"
    collapsible.rows <- CollapsibleRows(P)
    if(length(collapsible.rows) == 0) return(P)
    if(length(collapsible.rows) == 1) {
     return(CollapseRow(collapsible.rows, P))
    } else {
      r <- sample(collapsible.rows, 1)
      return(CollapseRow(r, P))
    }
  }

  ShrinkCol <- function(P){
  "Shrinks a row, if possible. If more than 1 row possible, selects at random"
    collapsible.cols <- CollapsibleCols(P)
    if(length(collapsible.cols) == 0) return(P)
    if(length(collapsible.cols) == 1) {
      # if there is only 1, it gets turned into a scalar. Ugh. 
      return(CollapseCol(collapsible.cols, P))
    } else {
      c <- sample(collapsible.cols, 1)
      return(CollapseCol(c, P))
    }
  }

  OK <- function(P, n){
    "Helper function useful when debugging shirnking"
    x <- as.vector(P)  
    length(sort(x[x !=0 ])) == n
  }

  ShrinkMatrix <- function(P){
    "Shrinks longest dimension first, stops when can't shrink on either dimension"
    num.rows <- dim(P)[1]
    num.cols <- dim(P)[2]
    if (num.rows > num.cols){
      P.smaller <- ShrinkRow(P)
      P.smaller <- ShrinkCol(P.smaller)
    } else {
      P.smaller <- ShrinkCol(P)
      P.smaller <- ShrinkRow(P.smaller)
    }
    if (all(dim(P) == dim(P.smaller))) {
      return(P.smaller)
    } else {
      return(ShrinkMatrix(P.smaller))
    }
  }

  # brute force to find a good shrunken matrix  
  best.dim <- Inf 
  P.best <- P
  for(i in 1:num.iterations){
    P.smaller = ShrinkMatrix(P)
    if(sum(dim(P.smaller)) < best.dim){
      P.best = P.smaller
      best.dim <- sum(dim(P.smaller))
    }
  }

  df.grid <- melt(P.best) %>% filter(value != 0)
  colnames(df.grid) <- c("new.row", "new.col", "index")
  df.combo <- df.grid %>% left_join(df)
  df.combo
}

# step 1: import create_new_grid() function (save in .GlobalEnv - paste & enter) (see above)
# step 2: go thru usage of geofacet here: https://gist.github.com/johnjosephhorton/571e81fa81e20ed72d770a62cb4d2dcb#file-geofacet_usage_example-r
# load following libraries
library(geofacet)
library(tidyverse)
library(ggrepel)
library(reshape2)   # dependency for melt() in create_new_grid()

# need to match province with ONE pair of longitude + latitude
# save thai_longlat_id to thai_longlat_id_unique
# save unique long lat id Region_Province
thai_longlat_id_unique <- thai_longlat_id

# long + lat ARE unique (this doesn't change anything)
thai_longlat_id_unique <- thai_longlat_id_unique %>% 
    distinct(long, lat, id, Region_Province .keep_all = TRUE)

# keep unique id, Region_Province (only) (n = 77 province left)
thai_longlat_id_unique <- thai_longlat_id_unique %>%
  distinct(id, Region_Province, .keep_all = TRUE)

# create data.frame(x, y, row, col, index, city)
n <- 77
x <- thai_longlat_id_unique$long      # num
y <- thai_longlat_id_unique$lat       # num
index <- thai_longlat_id_unique$id    # id already integer
province <- as.character(thai_longlat_id_unique$Region_Province)  #instead of city

# illustrate putting on n x n grid
# NOTE: difference between rank(x) or rank(y) vs x or y
row <- rank(y)
col <- rank(x)
grid_df <- data.frame(x, y, row, col, index, province)


# raw coordinates
ggplot(data = grid_df, aes(x=x,y=y)) 
+ geom_text_repel(aes(label=province)) 
+ theme_bw()

## key is turning x-y long-lat raw coordinates into 'rank' row + col

# on n x n grid
ggplot(data = grid_df, aes(x=col,y=row)) 
+ geom_tile(fill='grey', color = 'black') 
+ geom_text_repel(aes(label=province)) 
+ theme_bw()

## use create_new_grid() function to grid a reduced grid
library(reshape2)  # for melt() function dependency in create_new_grid()

reduce_grid <- create_new_grid(x,y,province, num.iterations = 500)


### NOTE: message Joining, by = "index"
## reduce_grid index NOT same as grid_df index

# step 3: create and save new grid
thai_grid_map <- ggplot(data = reduce_grid, aes(x = new.col, y = new.row)) 
  + geom_point() 
  + geom_tile(fill='grey', color = 'black') 
  + geom_text_repel(aes(label = unit.name), size = 2) 
  + theme_bw()

###########################################
## Use thai_grid_map in geofacet package ##
###########################################

### try user generated grid to show in grid_preview() function of geofaceet package
### very rough, geographically incorrect

reduce_grid2 <- reduce_grid %>%
  select(row, col, unit.name)

colnames(reduce_grid2)[3] <- 'name'
reduce_grid2[,4] <- 1:77
colnames(reduce_grid2)[4] <- 'code'
grid_preview(reduce_grid2)
reduce_grid2$code <- reduce_grid2$name

## second try now using new.col and new.rol generated in reduce_grid
## improvement, but Y-axis needs to be reversed

reduce_grid3 <- reduce_grid %>%
  select(new.row, new.col, unit.name, index)

colnames(reduce_grid3)[1] <- 'row'
colnames(reduce_grid3)[2] <- 'col'
colnames(reduce_grid3)[3] <- 'name'
colnames(reduce_grid3)[4] <- 'code' 

grid_preview(reduce_grid)

## Try reduce_grid3a (delete rows 52:77) to match number of rows in us_state_grid1
## Try copying us_state_grid1$code -> reduce_grid3a$code to match 'state' in state_unemp
reduce_grid3a <- reduce_grid3[-c(52:77),]
reduce_grid3a$code <- us_state_grid1$code

##### This works #####; note: using custom made "reduce_grid3a" works in facet_geo()
ggplot(state_unemp, aes(year, rate)) 
  + geom_line() 
  + facet_geo(~state, grid = reduce_grid3a, label = 'name')

## NOTE this error suggests 'state' column in state_unemp is *joined* by us_state_grid1
## Error: The values of the specified facet_geo column 'state' do not match any column of the specified grid.


########## Create new Grid with Geo Grid Designer ############

# Write data frame to a delimited file (Geo Grid Designer accepts this format)

# Final Thai Geo Grid Designer
# step 1: 
reduce_grid4 <- reduce_grid3

# step 2: (use 19 instead of 18 because all rows, col need to be positive number for Geo Grid Designer)
reduce_grid4$row <- 19 - reduce_grid4$row

# step 3: put this output in Geo Grid Designer
cat(format_csv(reduce_grid4))

# NOTE: code + state have to align 
ggplot(state_unemp, aes(year, rate)) 
  + geom_line() 
  + facet_geo(~state, grid = "us_state_grid1", label = 'name')

# NOTE: code + province for Thai map
ggplot(state_unemp, aes(year, rate)) 
  + geom_line() 
  + facet_geo(~state, grid = reduce_grid3a, label = 'name')

############### Fill Thai Geo Grid Map with Values ################

# note steps to update data from API

# cases: age, gender, province, nationality of patient
# NOTE: most cases in Bangkok (province), but there's also info on District 
# ambitious case would be to create a District Grid Map of Bangkok
cases = GET('https://covid19.th-stat.com/api/open/cases')
cases_data = fromJSON(rawToChar(cases$content))
df <- cases_data$Data

# timeline: (NOT geographic) NewConfirmed, NewRecovered, NewHospitalized, NewDeaths,
# Confirmed, Recovered, Hospitalized, Deaths
timeline = GET('https://covid19.th-stat.com/api/open/timeline')
timeline_data = fromJSON(rawToChar(timeline$content))
df2 <- timeline_data$Data

# next step(s)
# list out possible data viz within Grid (use state_unemp and us_state_grid1)
# See Also: election, india_pop, nhs_scot_dental, london_afford, sa_pop_dens, aus_pop, eu_gdp, state_ranks 

# time series, barchart, gdp percapital over time, geom_col(), geom_line() etc.

# see how df or df2 would need to be subsetted 

df$ConfirmDate # needs to be changed from character to date format; Posix
df$GenderEn # needs to be changed to a factor

#### NOTE: likely need to create NEW data frames
# Note: will need to change data type of ConfirmDate FIRST before further data wrangling is possible

# the dates below here are out of order because date is stored as "character"
View(df %>% group_by(ConfirmDate, GenderEn) %>% tally(sort = TRUE))


# Time Formatting: Character to POSIXlt
covidthai <- df
covidthai$ConfirmDate <- strptime(covidthai$ConfirmDate, "%Y-%m-%d %H:%M:%OS")
# must change to POSIXct format before plotting
covidthai$ConfirmDate <- as.POSIXct(covidthai$ConfirmDate)

# high likelihood that need to convert POSIXct into Date to work with GGPLOT
province_case2$ConfirmDate <- as.Date.POSIXct(province_case2$ConfirmDate)

# check to see if POSIXct date format suitable for plotting
View(covidthai %>% arrange(ConfirmDate))
View(covidthai %>% arrange(desc(ConfirmDate)))

##### New Data Frame province_case ######

# province_case has ConfirmDate, ProvinceEn and n (number of cases)
covidthai %>% group_by(ConfirmDate, ProvinceEn) %>% tally(sort = TRUE) -> province_case
# sort province_case by ConfirmDate Jan 12 - May 7
province_case %>% arrange(ConfirmDate) -> province_case
# (subsequent ste) sort province_case by ProvinceEn (alphabetically)
province_case %>% arrange(ProvinceEn) -> province_case
# NOTE the above two steps have to be done separately, sequentially
colnames(province_case)[3] <- "cases"

########## Thai Grid Map with COVID19 Cases #############

## Using province_case and reduce_grid4 / reduce_grid5
## Emulate: state_unemp and us_state_grid2

reduce_grid4 %>% arrange(name) -> reduce_grid5

## change code numbers to letters
# Singburi & Sisaket reversed

reduce_grid5$code <- c('ACR', 'ATG', 'BKK', 'BKN', 'BRM', 'CCO', 'CNT', 'CPM', 'CTI', 
                       'CMI', 'CRI', 'CBI', 'CPN', 'KSN', 'KPT', 'KRI', 'KKN', 'KBI',
                       'LPG', 'LPN', 'LEI', 'LRI', 'MSN', 'MKM', 'MDH', 'NYK', 'NPT', 
                       'NPM', 'NMA', 'NSN', 'NRT', 'NAN', 'NWT', 'NBP', 'NKI', 'NBI', 
                       'PTE', 'PTN', 'PNA', 'PLG', 'PYO', 'PNB', 'PBI', 'PCT', 'PLK', 
                       'AYA', 'PRE', 'PKT', 'PRI', 'PKN', 'RNG', 'RBR', 'RYG', 'RET', 
                       'SKW', 'SNK', 'SPK', 'SKN', 'SKM', 'SRI', 'STN', 'SSK', 'SBR', 
                       'SKA', 'STI', 'SPB', 'SNI', 'SRN', 'TAK', 'TRG', 'TRT', 'UBN', 
                       'UDN', 'UTI', 'UTD', 'YLA', 'YST')

# import letter codes for province_case
# note: need to bring it back to original df for streamlined future workflows
province_case[,"province"] <- NA

province_case$province <- ifelse(province_case$ProvinceEn=="Amnat Charoen", 'ACR', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Bangkok", 'BKK', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Buriram", 'BRM', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Chachoengsao", 'CCO', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Chaiyaphum", 'CPM', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Chanthaburi", 'CTI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Chiang Mai", 'CMI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Chiang Rai", 'CRI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Chonburi", 'CBI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Chumphon", 'CPN', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Kalasin", 'KSN', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Kanchanaburi", 'KRI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Khon Kaen", 'KKN', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Krabi", 'KBI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Lampang", 'LPG', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Lamphun", 'LPN', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Loei", 'LEI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Lopburi", 'LRI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Mae Hong Son", 'MSN', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Maha Sarakham", 'MKM', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Mukdahan", 'MDH', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Nakhon Nayok", 'NYK', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Nakhon Pathom", 'NPT', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Nakhon Phanom", 'NPM', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Nakhon Ratchasima", 'NMA', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Nakhon Sawan", 'NSN', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Nakhon Si Thammarat", 'NRT', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Narathiwat", 'NWT', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Nong Bua Lamphu", 'NBP', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Nong Khai", 'NKI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Nonthaburi", 'NBI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Pathum Thani", 'PTE', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Pattani", 'PTN', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Phang Nga", 'PNA', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Phatthalung", 'PLG', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Phayao", 'PYO', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Phetchabun", 'PNB', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Phetchaburi", 'PBI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Phitsanulok", 'PLK', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Phra Nakhon Si Ayutthaya", 'AYA', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Phrae", 'PRE', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Phuket", 'PKT', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Prachinburi", 'PRI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Prachuap Khiri Khan", 'PKN', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Ratchaburi", 'RBR', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Rayong", 'RYG', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Roi Et", 'RET', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Sa Kaeo", 'SKW', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Sakon Nakhon", 'SNK', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Samut Prakan", 'SPK', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Samut Sakhon", 'SKN', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Samut Songkhram", 'SKM', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Saraburi", 'SRI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Satun", 'STN', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Sisaket", 'SSK', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Songkhla", 'SKA', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Sukhothai", 'STI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Suphan Buri", 'SPB', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Surat Thani", 'SNI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Surin", 'SRN', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Tak", 'TAK', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Trang", 'TRG', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Ubon Ratchathani", 'UBN', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Udon Thani", 'UDN', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Uthai Thani", 'UTI', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Uttaradit", 'UTD', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Yala", 'YLA', province_case$province)
province_case$province <- ifelse(province_case$ProvinceEn=="Yasothon", 'YST', province_case$province)


## Draft of Thai Geo Grid
## ERROR: in as.POSIXct.numeric(value) : 'origin' must be supplied
## WARNING: Some values in the specified facet_geo column 'province' do not match the 'code' column 
## of the specified grid and will be removed: PNB


province_case2 <- province_case

# Getting Date in the Format that works with GGPLOT
province_case2$ConfirmDate <- as.Date.POSIXct(province_case2$ConfirmDate)

##### NOTE Differences between province_case vs province_case2
##### province_case is closer to the original covidthai data frame
str(province_case$ConfirmDate)   # POSIXct[1:632]
str(province_case2$ConfirmDate)  # Date[1:632]
str(covidthai$ConfirmDate)       # POSIXct[1:2993]
###### NOTE: POSIXct does not work with ggplot (while Date does), why i use province_case2 ######
###### Error: Invalid input: date_trans works with objects of class Date only #######



# First Draft
ggplot(province_case2, aes(ConfirmDate, cases)) 
  + geom_line() 
  + facet_geo(~ province, grid = reduce_grid5, label = 'name') 
  + scale_x_date(date_labels = "%b %y", date_breaks = "1 month") 
  + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'))


####### stretch reduce_grid_5
####### use Geo Grid Designer
grid_preview(reduce_grid5)

mygrid <- data.frame(
  row = c(1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 12, 12, 12, 13, 13, 13, 14, 15, 16, 17, 17, 18, 18, 19, 20, 20, 21, 21, 21),
  col = c(4, 9, 2, 1, 5, 8, 4, 3, 2, 1, 6, 10, 7, 9, 8, 5, 5, 3, 11, 10, 3, 6, 5, 2, 11, 8, 3, 10, 9, 11, 4, 8, 7, 5, 3, 10, 11, 5, 8, 7, 10, 9, 7, 6, 8, 5, 6, 3, 4, 5, 7, 9, 8, 3, 7, 5, 8, 9, 3, 8, 9, 3, 5, 4, 3, 3, 3, 4, 5, 4, 5, 5, 7, 5, 8, 7, 6),
  name = c("Chiang Rai", "Bungkan", "Chiang Mai", "Mae Hong Son", "Nan", "Nong Khai", "Phayao", "Phrae", "Lampang", "Lamphun", "Loei", "Nakhon Phanom", "Nong Bua Lam Phu", "Sakon Nakhon", "Udon Thani", "Uttaradit", "Phitsanulok", "Sukhothai", "Mukdahan", "Kalasin", "Kamphaeng Phet", "Phetchabun", "Phichit", "Tak", "Amnat Charoen", "Khon Kaen", "Nakhon Sawan", "Roi Et", "Maha Sarakham", "Si Sa Ket", "Chai Nat", "Chaiyaphum", "Lop Buri", "Sing Buri", "Uthai Thani", "Yasothon", "Ubon Ratchathani", "Ang Thong", "Nakhon Ratchasima", "Saraburi", "Surin", "Buri Ram", "Nakhon Nayok", "Phra Nakhon Si Ayutthaya", "Prachin Buri", "Suphan Buri", "Bangkok", "Kanchanaburi", "Nakhon Pathom", "Nonthaburi", "Pathum Thani", "Sa Kaeo", "Chachoengsao", "Ratchaburi", "Samut Prakan", "Samut Sakhon", "Chon Buri", "Chanthaburi", "Samut Songkhram", "Rayong", "Trat", "Chumphon", "Phetchaburi", "Prachuap Khiri Khan", "Ranong", "Surat Thani", "Phangnga", "Krabi", "Nakhon Si Thammarat", "Phuket", "Trang", "Phattalung", "Pattani", "Satun", "Narathiwat", "Songkhla", "Yala"),
  code = c("CRI", "BKN", "CMI", "MSN", "NAN", "NKI", "PYO", "PRE", "LPG", "LPN", "LEI", "NPM", "NBP", "SNK", "UDN", "UTD", "PLK", "STI", "MDH", "KSN", "KPT", "PNB", "PCT", "TAK", "ACR", "KKN", "NSN", "RET", "MKM", "SSK", "CNT", "CPM", "LRI", "SBR", "UTI", "YST", "UBN", "ATG", "NMA", "SRI", "SRN", "BRM", "NYK", "AYA", "PRI", "SPB", "BKK", "KRI", "NPT", "NBI", "PTE", "SKW", "CCO", "RBR", "SPK", "SKN", "CBI", "CTI", "SKM", "RYG", "TRT", "CPN", "PBI", "PKN", "RNG", "SNI", "PNA", "KBI", "NRT", "PKT", "TRG", "PLG", "PTN", "STN", "NWT", "SKA", "YLA"),
  stringsAsFactors = FALSE
)
geofacet::grid_preview(mygrid)

mygrid2 <- data.frame(
  row = c(1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 12, 12, 12, 13, 14, 15, 16, 17, 17, 17, 18, 19, 20, 20, 20, 21, 21),
  col = c(3, 2, 4, 3, 1, 9, 8, 4, 3, 5, 2, 10, 9, 8, 7, 6, 5, 4, 11, 2, 8, 4, 5, 10, 5, 7, 3, 9, 8, 6, 11, 9, 4, 7, 5, 3, 10, 5, 8, 6, 10, 4, 2, 9, 7, 5, 8, 3, 6, 6, 4, 5, 8, 7, 6, 5, 7, 8, 4, 3, 8, 9, 3, 3, 3, 3, 3, 4, 5, 3, 5, 5, 7, 5, 6, 8, 7),
  name = c("Chiang Rai", "Chiang Mai", "Nan", "Phayao", "Mae Hong Son", "Bungkan", "Nong Khai", "Phrae", "Lampang", "Uttaradit", "Lamphun", "Nakhon Phanom", "Sakon Nakhon", "Udon Thani", "Nong Bua Lam Phu", "Loei", "Phitsanulok", "Sukhothai", "Mukdahan", "Tak", "Kalasin", "Kamphaeng Phet", "Phichit", "Amnat Charoen", "Phetchabun", "Khon Kaen", "Nakhon Sawan", "Roi Et", "Maha Sarakham", "Chaiyaphum", "Ubon Ratchathani", "Si Sa Ket", "Chai Nat", "Lop Buri", "Sing Buri", "Uthai Thani", "Yasothon", "Ang Thong", "Nakhon Ratchasima", "Saraburi", "Surin", "Suphan Buri", "Kanchanaburi", "Buri Ram", "Nakhon Nayok", "Phra Nakhon Si Ayutthaya", "Prachin Buri", "Ratchaburi", "Pathum Thani", "Bangkok", "Nakhon Pathom", "Nonthaburi", "Sa Kaeo", "Chachoengsao", "Samut Prakan", "Samut Sakhon", "Chon Buri", "Chanthaburi", "Samut Songkhram", "Phetchaburi", "Rayong", "Trat", "Prachuap Khiri Khan", "Chumphon", "Ranong", "Surat Thani", "Phangnga", "Krabi", "Nakhon Si Thammarat", "Phuket", "Trang", "Phattalung", "Pattani", "Satun", "Songkhla", "Narathiwat", "Yala"),
  code = c("CRI", "CMI", "NAN", "PYO", "MSN", "BKN", "NKI", "PRE", "LPG", "UTD", "LPN", "NPM", "SNK", "UDN", "NBP", "LEI", "PLK", "STI", "MDH", "TAK", "KSN", "KPT", "PCT", "ACR", "PNB", "KKN", "NSN", "RET", "MKM", "CPM", "UBN", "SSK", "CNT", "LRI", "SBR", "UTI", "YST", "ATG", "NMA", "SRI", "SRN", "SPB", "KRI", "BRM", "NYK", "AYA", "PRI", "RBR", "PTE", "BKK", "NPT", "NBI", "SKW", "CCO", "SPK", "SKN", "CBI", "CTI", "SKM", "PBI", "RYG", "TRT", "PKN", "CPN", "RNG", "SNI", "PNA", "KBI", "NRT", "PKT", "TRG", "PLG", "PTN", "STN", "SKA", "NWT", "YLA"),
  stringsAsFactors = FALSE
)
geofacet::grid_preview(mygrid2)

mygrid3 <- data.frame(
  row = c(2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 10, 10, 10, 11, 11, 12, 13, 14, 15, 16, 17, 17, 17, 18, 19, 20, 20, 20, 21, 21),
  col = c(3, 2, 3, 4, 1, 9, 8, 2, 5, 3, 2, 10, 9, 8, 7, 6, 5, 4, 9, 2, 8, 3, 4, 6, 7, 5, 10, 11, 3, 9, 8, 6, 7, 4, 5, 10, 8, 4, 3, 6, 7, 9, 5, 9, 3, 2, 5, 7, 8, 4, 6, 3, 4, 7, 8, 5, 6, 4, 7, 8, 3, 9, 3, 3, 3, 3, 3, 4, 5, 3, 5, 5, 7, 5, 6, 8, 7),
  name = c("Chiang Rai", "Chiang Mai", "Phayao", "Nan", "Mae Hong Son", "Bungkan", "Nong Khai", "Lampang", "Uttaradit", "Phrae", "Lamphun", "Nakhon Phanom", "Sakon Nakhon", "Udon Thani", "Nong Bua Lam Phu", "Loei", "Phitsanulok", "Sukhothai", "Mukdahan", "Tak", "Kalasin", "Kamphaeng Phet", "Phichit", "Chaiyaphum", "Khon Kaen", "Phetchabun", "Amnat Charoen", "Ubon Ratchathani", "Nakhon Sawan", "Roi Et", "Maha Sarakham", "Saraburi", "Lop Buri", "Sing Buri", "Ang Thong", "Yasothon", "Si Sa Ket", "Chai Nat", "Uthai Thani", "Pathum Thani", "Nakhon Ratchasima", "Buri Ram", "Nonthaburi", "Surin", "Suphan Buri", "Kanchanaburi", "Bangkok", "Nakhon Nayok", "Prachin Buri", "Phra Nakhon Si Ayutthaya", "Samut Prakan", "Ratchaburi", "Nakhon Pathom", "Chachoengsao", "Sa Kaeo", "Samut Sakhon", "Chon Buri", "Samut Songkhram", "Rayong", "Chanthaburi", "Phetchaburi", "Trat", "Prachuap Khiri Khan", "Chumphon", "Ranong", "Surat Thani", "Phangnga", "Krabi", "Nakhon Si Thammarat", "Phuket", "Trang", "Phattalung", "Pattani", "Satun", "Songkhla", "Narathiwat", "Yala"),
  code = c("CRI", "CMI", "PYO", "NAN", "MSN", "BKN", "NKI", "LPG", "UTD", "PRE", "LPN", "NPM", "SNK", "UDN", "NBP", "LEI", "PLK", "STI", "MDH", "TAK", "KSN", "KPT", "PCT", "CPM", "KKN", "PNB", "ACR", "UBN", "NSN", "RET", "MKM", "SRI", "LRI", "SBR", "ATG", "YST", "SSK", "CNT", "UTI", "PTE", "NMA", "BRM", "NBI", "SRN", "SPB", "KRI", "BKK", "NYK", "PRI", "AYA", "SPK", "RBR", "NPT", "CCO", "SKW", "SKN", "CBI", "SKM", "RYG", "CTI", "PBI", "TRT", "PKN", "CPN", "RNG", "SNI", "PNA", "KBI", "NRT", "PKT", "TRG", "PLG", "PTN", "STN", "SKA", "NWT", "YLA"),
  stringsAsFactors = FALSE
)
geofacet::grid_preview(mygrid3)

### Second Draft visualize grid map with total_cases
province_case %>% group_by(ProvinceEn, province) %>% summarize(sum_cases = sum(cases)) -> total_cases

## create new data frame that contains row, cols for the grid 
## and values to visualize
colnames(total_cases)[2] <- 'code'

mygrid3_cases <- mygrid3 %>%
  inner_join(total_cases, by = 'code')

## SECOND DRAFT of Thai Grid Map with sum_cases
## done in GGPLOT once data for Grid is available - NOT in facet_geo()
## Tutorial: https://www.maartenlambrechts.com/2017/10/22/tutorial-a-worldtilegrid-with-ggplot2.html

ggplot(mygrid3_cases, aes(xmin = col, ymin = row, xmax = col + 1, ymax = row + 1, fill = as.factor(sum_cases))) 
+ geom_rect(color = '#ffffff') 
+ theme_minimal() 
+ theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank()) 
+ geom_text(aes(x = col, y = row, label = code), color = '#ffffff', alpha = 0.5, nudge_x = 0.5, nudge_y = -0.5, size = 3) 
+ scale_y_reverse() 
+ scale_fill_hue(l=10, c=1543)

## need to create color 'factor' to help visualize continuous data with outlier (BKK)
# get summary statistics, filter out BKK (outlier)
mygrid3_cases %>% filter(code != 'BKK') %>% summary(sum_cases)

# Mean   : 21.64 | Median :  6.00 | 1st Qu.:  3.00 | 3rd Qu.: 17.00

## Proposed Factors 1-3 (First Quartile), 4-6 (Second Quartile), 
## 7-17 (Third Quartile), 18 - 220 (Fourth Quartile), BKK (BKK)

# 1-3 (First Quartile)
mygrid3_cases$color <- ifelse(mygrid3_cases$sum_cases < 4, 'q1', mygrid3_cases$color)
# 4-6 (Second Quartile)
mygrid3_cases$color <- ifelse(mygrid3_cases$sum_cases > 3 & mygrid3_cases$sum_cases < 7, 'q2', mygrid3_cases$color)
# 7-17 (Third Quartile)
mygrid3_cases$color <- ifelse(mygrid3_cases$sum_cases > 6 & mygrid3_cases$sum_cases < 18, 'q3', mygrid3_cases$color)
# 18 - 220 (Fourth Quartile)
mygrid3_cases$color <- ifelse(mygrid3_cases$sum_cases > 17 & mygrid3_cases$sum_cases < 221, 'q4', mygrid3_cases$color)
# BKK (Outlier)
mygrid3_cases$color[40] <- 'bkk'

mygrid3_cases$color <- as.factor(mygrid3_cases$color)

## THIRD VERSION Thai Grid Map with colors as factors
ggplot(mygrid3_cases, aes(xmin = col, ymin = row, xmax = col + 1, ymax = row + 1, fill = color)) 
  + geom_rect(color = '#ffffff') 
  + theme_minimal() 
  + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank()) 
  + geom_text(aes(x = col, y = row, label = code), color = 'black', alpha = 0.8, nudge_x = 0.5, nudge_y = -0.5, size = 3) 
  + scale_y_reverse() 
  + scale_fill_manual(values = c("#bd0026", '#ffffb2', '#fecc5c', '#fd8d3c', '#f03b20'))
  + theme(legend.position = 'none')
  + labs(title = "Covid19 cases throughout Thailand", subtitle = 'January - May, 2020')

## create column for provincial codes in covidthai
province_case %>%
+ select(ConfirmDate, ProvinceEn, province) -> province_codes

colnames(province_codes)[3] <- 'code'

covidthai2 <- covidthai %>%
+ inner_join(province_codes, by = c('ProvinceEn', 'ConfirmDate'))

### province_by_gender: create data frame (like total_cases) but for GenderEn
covidthai2 %>% group_by(ProvinceEn, GenderEn, code) %>% tally(sort = TRUE) -> province_by_gender

colnames(province_by_gender)[4] <- 'cases'

mygrid3_gender <- province_by_gender %>%
+ inner_join(mygrid3, by = 'code')

# First draft Thai Grid Map (Male) - take out BKK (outlier) GGPLOT VERSION
mygrid3_gender %>% 
  filter(GenderEn=='Male') %>% 
  filter(code != 'BKK') %>% 
  ggplot(aes(xmin = col, ymin = row, xmax = col + 1, ymax = row + 1, fill = cases)) 
  + geom_rect(color = '#ffffff') 
  + theme_minimal() 
  + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank()) 
  + geom_text(aes(x = col, y = row, label = code), color = '#ffffff', alpha = 0.5, nudge_x = 0.5, nudge_y = -0.5, size = 3) 
  + scale_y_reverse() 
  + scale_fill_viridis_c()

# Draft of GEOFACET VERSION
mygrid3_gender %>% 
  filter(code != 'BKK') %>% 
  ggplot(aes(x=GenderEn, y=cases, fill=GenderEn)) 
  + geom_col(position = position_dodge()) 
  # using mygrid3 dataframe
  + facet_geo(~ code, grid = mygrid3, label = "code") 
  + theme(strip.text.x = element_text(size = 4))

# NOTE: add theme(panel.margin = unit(1, 'lines')) for slightly better spacing

# GeoFacet Cases by Gender Version 2.0
mygrid3_gender %>% 
  filter(code != 'BKK') %>% 
  ggplot(aes(x=GenderEn, y=cases, fill=GenderEn)) 
  + geom_col(position = position_dodge()) 
  + facet_geo(~ code, grid = mygrid3, label = "code", scales = 'free_y') 
  + theme(strip.text.x = element_text(size = 8), axis.text.y = element_text(size = 6)) 
  + scale_y_continuous( breaks = round(seq(min(mygrid3_gender$cases), max(mygrid3_gender$cases)))  )


# GeoFacet Cases by Gender version 3.0
mygrid3_gender %>% 
  filter(code != 'BKK') %>% 
  ggplot(aes(x=GenderEn, y=cases, fill=GenderEn)) 
  + geom_bar(position = 'dodge', stat = 'identity') 
  + geom_text(aes(label=cases), position = position_dodge(0.5), size = 2.5, vjust=1.25) 
  + facet_geo(~ code, grid = mygrid3, label = 'code', scales = 'free_y') 
  + theme(strip.text.x = element_text(size = 6), axis.text.y = element_blank())


# GeoFacet Cases by Age (factor) (grid = mygrid3)
# NOTE: try grid = reduce_grid5 (old grid - does NOT look like Thailand)
mygrid3_age_fct %>% 
  filter(code != 'BKK') %>% 
  ggplot(aes(x=age_fct, y=cases, fill=age_fct)) 
  + geom_bar(position = 'dodge', stat = 'identity') 
  + geom_text(aes(label=cases), position = position_dodge(0.5), size = 2.5, vjust=1.25) 
  + facet_geo(~ code, grid = mygrid3, label = 'code', scales = 'free_y') 
  + theme(strip.text.x = element_text(size = 6), axis.text.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, color = 'black', size=4))

#################################################################

# create draft of Thai province (consider Bangkok District map)
library(rgdal)
library(broom) # get long and lat

# download BMASubDistrict_Polygon folder (whole folder) into working directory
###### 180 SUBDISTRICTS
# must contain: .shp, .shx, .dbf files
# layer = is file name infront of .shp , .shx .dbf 
bkk <- readOGR("./BMASubDistrict_Polygon", layer = 'BMA_ADMIN_SUB_DISTRICT')
View(bkk)

# quick plot in base R
plot(bkk, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )

# IMPORTANT - to get longitude and lattitude you need convert to data frame
# using broom package
library(broom)

bkk_fortified <- tidy(bkk)

# more detailed plot with ggplot()
ggplot() 
  + geom_polygon(data = bkk_fortified, aes(x = long, y = lat, group = group), fill="#69b3a2", color="white") 
  + theme_void()


##### 50 DISTRICTS
### Download 50 district folder
## must contain: .shp, .shx, .dbf files
# layer = is file name infront of .shp , .shx .dbf 
library(rgdal)
library(broom)

district <- readOGR("./district", layer = 'district')
district_fortified <- tidy(district)

ggplot() 
  + geom_polygon(data = district_fortified, aes(x = long, y = lat, group = group), fill="#69b3a2", color="white") 
  + theme_void()

district_fortified_unique <- district_fortified %>%
+ distinct(id, .keep_all = TRUE)

######### create BANGKOK DISTRICT geo facet grid

# create data.frame(x, y, row, col, index, city)
bkk_n <- 50
bkk_x <- district_fortified_unique$long
bkk_y <- district_fortified_unique$lat
bkk_index <- district_fortified_unique$id
bkk_district <- as.character(district_fortified_unique$id)

# illustrate putting on n x n grid
# NOTE: difference between rank(x) or rank(y) vs x or y
bkk_row <- rank(bkk_y)
bkk_col <- rank(bkk_x)
bkk_grid_df <- data.frame(bkk_x, bkk_y, bkk_row, bkk_col, bkk_index, bkk_district)

# on n x n grid
ggplot(data = bkk_grid_df, aes(x=bkk_col,y=bkk_row)) 
  + geom_tile(fill='grey', color = 'black') 
  + geom_text_repel(aes(label=bkk_district)) 
  + theme_bw()


# reduce_grid
bkk_reduce_grid <- create_new_grid(bkk_x, bkk_y, bkk_district, num.iterations = 500)

# bkk_district_grid_map
bkk_district_grid_map <- ggplot(data = bkk_reduce_grid, aes(x = new.col, y = new.row)) 
  + geom_point() 
  + geom_tile(fill='grey', color='black') 
  + geom_text_repel(aes(label = unit.name), size = 2) 
  + theme_classic()

####### PROBLEM - need to match 'id' in district_fortified_unique with actual DISTRICT NAMES
### bkk_district <- as.character(district_fortified_unique$id) needs to be actual DISTRICT NAMES


## develop dataframe that matches 'id' in district_fortified with AVERAGE lat and long
## Important because need a way to match 'id' to actual district names in this file:
## https://commons.wikimedia.org/wiki/File:Khet_Bangkok.svg
## save to text_centroid
district_fortified %>% 
  group_by(id) %>% 
  summarize(clat = mean(lat), clong = mean(long)) -> text_centroid

## compare with bkk_reduce_grid
## text_centroid aligns with unit.name in bkk_reduce_grid
## manually add District Names next to unit.name in bkk_reduce_grid

> bkk_reduce_grid[1,9] <- 'Bang Khun Thian'
> bkk_reduce_grid[2,9] <- 'Bang Bon'
> bkk_reduce_grid[3,9] <- 'Nong Khaem'
> bkk_reduce_grid[4,9] <- 'Phasi Charoen'
