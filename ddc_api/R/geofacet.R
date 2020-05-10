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

# create draft of Thai province (consider Bangkok District map)
# consider submitting reduce_grid_4 to geofacet() team
