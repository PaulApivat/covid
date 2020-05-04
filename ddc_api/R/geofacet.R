## steps

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

## open previous Thai map
## find lat and long

# key is connecting lat/long to row/column to name of province/city

# see if you can get lat + long a country level (Thailand)
# see if you can get lat + long at city level (Bangkok)



