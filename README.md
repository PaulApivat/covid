# Covid-19

A repository for Covid-19 related projects. I'm based in Bangkok, Thailand so several projects focus on data specific to this region.

## Bangkok, the Outlier

This project, located in the `ddc_api/R` directory, visualizes confirmed cases of Covid-19 in Thailand to show how much of the story is *really* about Bangkok. 

### Process

Note: Update data from Jan 12 - 
1. Fetch data from [Department of Disease Control Open API](https://covid19.th-stat.com/th/api)
2. Data transformation to dataframe (df)
3. Transform df to mygrid3_cases_alt
4. Use GeoFacet package to custom make Thailand Grid Map (also Bangkok District Grid Map) see `mygrid3`
- see `create_new_grid` function from [here](https://github.com/johnjosephhorton/geofacet/blob/master/R/create_new_grid.R)
- import `thai_latlong_id` data frame from previous project
- use `create_new_grid` to use `thai_latlong_id` into `reduce_grid`
- use [Geo Grid Designer](https://hafen.github.io/grid-designer/) to customize until grid visually represents your country (see `mygrid3`)

5. Join `mygrid3` with latest `df` turn `df_sum`
- also join with `region_clusters`

6. Draw inspiration from Cedric Scherer's [tutorial](https://cedricscherer.netlify.app/2019/05/17/the-evolution-of-a-ggplot-ep.-1/)
- group each province into regional clusters
- add regional averages to each cluster
- save plots in layers
- use set.seed(123) to fix data points on plot (used with annotations)

7. Zoom in on Bangkok
- use previously created Geo Facet Grid (mybkkgrid2)
- 

## Dashboard

Located in the `flexdashboard` directory, this tool tracked daily confirmed cases, deaths and number of patients under investigation (PUI). Testing data is not made publicly available in Thailand so PUI serves as a proxy. No longer updated since we've reached 0 new cases (or single digit cases) for nearly two weeks. 

## Hospital Dashboard

A collaboration with [Dr. Tim Dechai](https://www.linkedin.com/in/tim-decha-952b4b1a7/) to create an educational tool for how hospitals might manage their resources in case of a pandemic breakout. This project is located in the `hospital_dash` directory.

### Datasets Source
Two datasets were downloaded from [Our World in Data](https://ourworldindata.org/coronavirus-source-data).

Dates are from Jan 21st - March 17th, 2020

full_data.csv
new_cases.csv

*Update* March 20th, 2020 [Bangkok Time]

As of last night, the team at Our World in Data made a decision to stop using World Health Organization (WHO) data in favor of the European Center for Disease Control (ECDC) data. You can read about their rationale in the link above.

Two data sets are available for comparison:

full_data.csv [Newly updated from ECDC as of March 19th]
who_full_data.csv [Previous dataset from WHO last updated March 17th]

