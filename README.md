---
output:
  html_document: default
  pdf_document: default
---
<!--[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/TheTS/palmsplusr?branch=master&svg=true)](https://ci.appveyor.com/project/TheTS/palmsplusr)-->
<!--[![Travis-CI Build Status](https://travis-ci.org/TheTS/palmsplusr.svg?branch=master)](https://travis-ci.org/TheTS/palmsplusr) -->
<!--[![codecov](https://codecov.io/gh/TheTS/actigraph.sleepr/branch/master/graph/badge.svg)](https://codecov.io/gh/TheTS/actigraph.sleepr)-->
[![Project Status: Initial development is in progress.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![packageversion](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg)](commits/master) [![Last-changedate](https://img.shields.io/badge/last%20change-2017--12--12-yellowgreen.svg?style=flat-square)](/commits/master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
palmsplusr: Accelerometer and GPS data processing for physical activity and transportation research
===================================================================================================

**palmsplusr** is an extension to the *Personal Activity Location Measurement System*, commonly known as [PALMS](https://ucsd-palms-project.wikispaces.com/). The purpose of **palmsplusr** is to provide a platform for researchers to enhance, combine, process, and analyse datasets produded by PALMS. This project is based on the [palmsplus](https://github.com/bsnizek/palmsplus) project originally written in SQL.

### Installation

``` r
library("devtools")
install_github("TheTS/palmslusr")
```

The **palmsplusr** workflow is presented in the figure below. The PALMS dataset, along with various other input files are used to build the palmsplus table. The palmsplus table is simply the dataset obtained from PALMS with several additional columns. Once palmsplus is built, it can be summarised two ways: as days or as trajectories. Days provides a summary of data per day, while trajectories builds and summarises individual trips.

![Palms Workflow](https://i.imgur.com/oDXd5vL.png)

### Loading the PALMS dataset

PALMS datasets are read in as a csv file, before being converted to a spatial object (known as a "simple feature").

``` r
suppressMessages(library(palmsplusr))
library(readr)
library(sf)
#> Linking to GEOS 3.6.1, GDAL 2.2.0, proj.4 4.9.3

palms <- read_csv(system.file("extdata", "one_participant.csv", package = "palmsplusr"))
#> Parsed with column specification:
#> cols(
#>   identifier = col_character(),
#>   palms_datetime = col_datetime(format = ""),
#>   dow = col_integer(),
#>   lat = col_double(),
#>   lon = col_double(),
#>   fixtypecode = col_integer(),
#>   iov = col_integer(),
#>   tripnumber = col_integer(),
#>   triptype = col_integer(),
#>   tripmot = col_integer(),
#>   activity = col_integer(),
#>   activityintensity = col_integer(),
#>   activityboutnumber = col_integer(),
#>   sedentaryboutnumber = col_integer()
#> )
palms <- st_as_sf(palms, coords = c("lon", "lat"), crs = 4326)
```

### Building palmsplus

The *palmsplus* build process essentially adds additional columns to the input PALMS dataset. However, *palmsplusr* needs to be told what columns to add, and how to calculate them. This is achieved by building a fields table, as demonstrated below:

``` r
palms_add_field("duration",   "1", TRUE)
palms_add_field("weekday",    "dow < 6")
palms_add_field("weekend",    "dow > 5")
palms_add_field("indoors",    "iov == 3", TRUE)
palms_add_field("outdoors",   "iov == 1", TRUE)
palms_add_field("in_vehicle", "iov == 2")
palms_add_field("inserted",   "fixtypecode == 6")
palms_add_field("pedestrian", "tripmot == 1")
palms_add_field("bicycle",    "tripmot == 2")
palms_add_field("vehicle",    "tripmot == 3")
palmsplus_fields
#> # A tibble: 10 x 3
#>          name          formula domain_field
#>         <chr>            <chr>        <chr>
#>  1   duration                1         TRUE
#>  2    weekday          dow < 6        FALSE
#>  3    weekend          dow > 5        FALSE
#>  4    indoors         iov == 3         TRUE
#>  5   outdoors         iov == 1         TRUE
#>  6 in_vehicle         iov == 2        FALSE
#>  7   inserted fixtypecode == 6        FALSE
#>  8 pedestrian     tripmot == 1        FALSE
#>  9    bicycle     tripmot == 2        FALSE
#> 10    vehicle     tripmot == 3        FALSE
```

The variables contained in the formula can be from the input palms dataset, or from other files you load in. Below is an example of reading in a shapefile, and adding a new field that checks whether a palms point is inside a polygon:

``` r
home <- read_sf(system.file("extdata/shapefiles/", "home.shp", package = "palmsplusr"))
home.buffer <- palms_buffer(point = home, distance = 100)

palms_add_field("at_home",   "palms_in_polygon(., filter(home.buffer, identifier == i), \"identifier\")")
```

Once all of the fields have been added, you can build the *palmsplus* table.

``` r
palmsplus <- palms_calc_palmsplus(palms)
#> Computed palmsplus for: BC0627
names(palmsplus)
#>  [1] "identifier"          "palms_datetime"      "dow"                
#>  [4] "fixtypecode"         "iov"                 "tripnumber"         
#>  [7] "triptype"            "tripmot"             "activity"           
#> [10] "activityintensity"   "activityboutnumber"  "sedentaryboutnumber"
#> [13] "duration"            "weekday"             "weekend"            
#> [16] "indoors"             "outdoors"            "in_vehicle"         
#> [19] "inserted"            "pedestrian"          "bicycle"            
#> [22] "vehicle"             "at_home"             "geometry"
```

### Building days

When building the days table, the data are summarised across sereral *domains*. Domains are added in the same way fields are added. Note that domains needs to be added before palmsplus is built.

``` r
palms_add_domain("d_home", "at_home")
palms_add_domain("d_transport", "pedestrian | bicycle | vehicle")

palmsplus <- palms_calc_palmsplus(palms)
#> Computed palmsplus for: BC0627
names(palmsplus)
#>  [1] "identifier"          "palms_datetime"      "dow"                
#>  [4] "fixtypecode"         "iov"                 "tripnumber"         
#>  [7] "triptype"            "tripmot"             "activity"           
#> [10] "activityintensity"   "activityboutnumber"  "sedentaryboutnumber"
#> [13] "duration"            "weekday"             "weekend"            
#> [16] "indoors"             "outdoors"            "in_vehicle"         
#> [19] "inserted"            "pedestrian"          "bicycle"            
#> [22] "vehicle"             "at_home"             "d_home"             
#> [25] "d_transport"         "geometry"
```

Notice there are two additional columns in the palmsplus table. These are used as aggregation domains when the *days* table is built.

``` r
days <- palms_calc_days(palmsplus)
head(days)
#> # A tibble: 6 x 8
#>   identifier       date d_home_duration d_home_indoors d_home_outdoors
#>        <chr>     <date>           <dbl>          <dbl>           <dbl>
#> 1     BC0627 2013-08-26          348.50              0           79.50
#> 2     BC0627 2013-08-27          929.25              0          532.00
#> 3     BC0627 2013-08-28          651.50              0          494.75
#> 4     BC0627 2013-08-29          353.75              0           57.75
#> 5     BC0627 2013-08-30         1440.00              0          983.50
#> 6     BC0627 2013-08-31         1350.50              0          863.25
#> # ... with 3 more variables: d_transport_duration <dbl>,
#> #   d_transport_indoors <dbl>, d_transport_outdoors <dbl>
```

### Building trajectories

Like previously, various fields can be calculated for each trajectory. The variables used in the formulas should be present in the palmsplus table.

``` r
palms_add_trajectory_field("mot",       "first(tripmot)")
palms_add_trajectory_field("wear",      "sum(activityintensity >= 0) * epoch")
palms_add_trajectory_field("sedentary", "sum(activityintensity == 0) * epoch")
palms_add_trajectory_field("light",     "sum(activityintensity == 1) * epoch")
palms_add_trajectory_field("moderate",  "sum(activityintensity == 2) * epoch")
palms_add_trajectory_field("vigorous",  "sum(activityintensity == 3) * epoch")
palms_add_trajectory_field("mvpa",      "moderate + vigorous")
```

``` r
trajectories <- palms_calc_trajectories(palmsplus)
head(trajectories)
#> Simple feature collection with 6 features and 9 fields
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: 174.6973 ymin: -36.80079 xmax: 174.7013 ymax: -36.79289
#> epsg (SRID):    4326
#> proj4string:    +proj=longlat +datum=WGS84 +no_defs
#>   identifier tripnumber mot wear sedentary light moderate vigorous mvpa
#> 1     BC0627          1   1  180        30    45        0      105  105
#> 2     BC0627          2   1  165         0    75       45       45   90
#> 3     BC0627          3   1  555        60   285      150       60  210
#> 4     BC0627          4   1  255        60   135       45       15   60
#> 5     BC0627          5   1  675         0   435      195       45  240
#> 6     BC0627          6   1 1230        90   195      510      435  945
#>                         geometry
#> 1 LINESTRING (174.69962969 -3...
#> 2 LINESTRING (174.69970838 -3...
#> 3 LINESTRING (174.70103566 -3...
#> 4 LINESTRING (174.69827459 -3...
#> 5 LINESTRING (174.69794882 -3...
#> 6 LINESTRING (174.69952199 -3...
```

It is also possible to isolate trjectories that start and end at specific locations. This is done by adding trip start end end criteria before building the trajectories table. Here is an example of trips starting and ending at home. Notice how the "in\_home" criteria was created above in the palmsplus table. All the start and end criteria should be pre-calculated to improve performance.

``` r
palms_add_trip_location("home_home", "at_home", "at_home")
trajectories <- palms_calc_trajectories(palmsplus)
head(trajectories)
#> Simple feature collection with 6 features and 10 fields
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: 174.6973 ymin: -36.80079 xmax: 174.7013 ymax: -36.79289
#> epsg (SRID):    4326
#> proj4string:    +proj=longlat +datum=WGS84 +no_defs
#>   identifier tripnumber mot wear sedentary light moderate vigorous mvpa
#> 1     BC0627          1   1  180        30    45        0      105  105
#> 2     BC0627          2   1  165         0    75       45       45   90
#> 3     BC0627          3   1  555        60   285      150       60  210
#> 4     BC0627          4   1  255        60   135       45       15   60
#> 5     BC0627          5   1  675         0   435      195       45  240
#> 6     BC0627          6   1 1230        90   195      510      435  945
#>   home_home                       geometry
#> 1         0 LINESTRING (174.69962969 -3...
#> 2         0 LINESTRING (174.69970838 -3...
#> 3         0 LINESTRING (174.70103566 -3...
#> 4         0 LINESTRING (174.69827459 -3...
#> 5         0 LINESTRING (174.69794882 -3...
#> 6         0 LINESTRING (174.69952199 -3...
```

### Summary plot

``` r
# TODO
```
