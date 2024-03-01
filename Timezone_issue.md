Timezone issue
================
Gergely Som
2024-03-01

\#Aims and goals I am trying to study the accessibility of social
services in Budapest by public transport, but I cannot seem to get the
different funcitons within the r5r envirnment to include transportation
mode other than walking. Here is a documentation of the issue.

\#Load Libraries and Setup First, we load the necessary libraries and
set the Java Home environment variable. Note that the rJava package must
be loaded after setting the JAVA_HOME environment variable.

``` r
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk-11')

# Load libraries
library(r5r)
```

    ## Warning: package 'r5r' was built under R version 4.2.3

    ## Please make sure you have already allocated some memory to Java by running:
    ##   options(java.parameters = '-Xmx2G').
    ## You should replace '2G' by the amount of memory you'll require. Currently, Java memory is set to

``` r
library(rJava)
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

    ## Keep up to date with changes at https://www.tidyverse.org/blog/

``` r
library(sf)
```

    ## Linking to GEOS 3.9.1, GDAL 3.4.3, PROJ 7.2.1; sf_use_s2() is TRUE

``` r
library(osmextract)
```

    ## Warning: package 'osmextract' was built under R version 4.2.3

    ## Data (c) OpenStreetMap contributors, ODbL 1.0. https://www.openstreetmap.org/copyright.
    ## Check the package website, https://docs.ropensci.org/osmextract/, for more details.

``` r
library(osmdata)
```

    ## Data (c) OpenStreetMap contributors, ODbL 1.0. https://www.openstreetmap.org/copyright

\#Build Transport Network We specify the data path and initialize the R5
transport network.

``` r
options(java.parameters = "-Xmx8g")

.jinit()

# Specify the data path
your_data_path <- "C:/UCL/Dissertation/transit_data"

list.files("C:/UCL/Dissertation/transit_data")
```

    ## [1] "budapest_gtfs2.zip"             "hungary-latest.gpkg"            "hungary-latest.osm.pbf"        
    ## [4] "hungary-latest.osm.pbf.mapdb"   "hungary-latest.osm.pbf.mapdb.p" "network.dat"                   
    ## [7] "network_settings.json"

``` r
# Initialize the R5 transport network
r5r_core <- setup_r5(data_path = your_data_path, verbose = FALSE)
```

    ## No raster .tif files found. Using elevation = 'NONE'.

    ## Using cached R5 version from C:/Users/somge/AppData/Local/R/win-library/4.2/r5r/jar/r5-v6.9-all.jar

    ## 
    ## Using cached network.dat from C:/UCL/Dissertation/transit_data/network.dat

\#Define Origin and Destination Here, we define the origin and
destination coordinates, create sf objects for each, and add an ‘id’
column. For faster run times since I have 48 hospital points as
destinations and 1137 hexagons tessellated from Budapest’s area that ha
sbeen centroidised, so that they are in the correct format for r5r
functions.There are definitely multiple modes of public transport to get
from the handpicked origin coodinates to the destination coordinate, so
there is no reason why it should only return mode “walk”.

``` r
# Define the coordinates
origin_coords <- matrix(c(19.05881, 47.51444), ncol = 2) # longitude, latitude
destination_coords <- matrix(c(19.06758, 47.50186), ncol = 2)

# Create sf objects for origin and destination
origin_sf <- st_sf(geom = st_sfc(st_point(origin_coords), crs = 4326))
destination_sf <- st_sf(geom = st_sfc(st_point(destination_coords), crs = 4326))

# Add an 'id' column to both sf objects
origin_sf$id <- "origin_1"
destination_sf$id <- "destination_1"
```

\#Set Timezone and Run Detailed Itineraries We set the timezone to
“Europe/Budapest” and run the detailed_itineraries function.

``` r
Sys.setenv(TZ = "Europe/Budapest")
.jinit(parameters = "-Duser.timezone=Europe/Budapest")
```

    ## [1] 0

``` r
# Set the departure time
departure_datetime <- as.POSIXct("2023-11-14 12:00:00", tz = "Europe/Budapest", format = "%Y-%m-%d %H:%M:%S")

# Run the detailed_itineraries function
test_itineraries <- detailed_itineraries(
  r5r_core = r5r_core,
  origins = origin_sf,
  destinations = destination_sf,
  mode = c("TRANSIT", "WALK"),
  departure_datetime = departure_datetime,
  max_trip_duration = 120,
  time_window = 30,
  suboptimal_minutes = 15,
  all_to_all = FALSE,
  shortest_path = FALSE,
  drop_geometry = FALSE
)

# Examine the output
str(test_itineraries)
```

    ## Classes 'sf', 'data.table' and 'data.frame': 1 obs. of  17 variables:
    ##  $ from_id         : chr "origin_1"
    ##  $ from_lat        : num 47.5
    ##  $ from_lon        : num 19.1
    ##  $ to_id           : chr "destination_1"
    ##  $ to_lat          : num 47.5
    ##  $ to_lon          : num 19.1
    ##  $ option          : int 1
    ##  $ departure_time  : chr "12:00:00"
    ##  $ total_duration  : num 30.1
    ##  $ total_distance  : int 1791
    ##  $ segment         : int 1
    ##  $ mode            : chr "WALK"
    ##  $ segment_duration: num 30.1
    ##  $ wait            : num 0
    ##  $ distance        : int 1791
    ##  $ route           : chr ""
    ##  $ geometry        :sfc_LINESTRING of length 1; first list element:  'XY' num [1:60, 1:2] 19.1 19.1 19.1 19.1 19.1 ...
    ##  - attr(*, "sf_column")= chr "geometry"
    ##  - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA NA NA NA NA ...
    ##   ..- attr(*, "names")= chr [1:16] "from_id" "from_lat" "from_lon" "to_id" ...

``` r
unique(test_itineraries$mode)
```

    ## [1] "WALK"

This output indicates that it only it has only recognised walking as a
mode of transport between our origin-destination pair. I also returns an
error message regarding timezone discrepancies even though the timezone
of the R environment was set to be GMT+1 the same as that fo the gtfs
files.

This is the agency.txt in my gtfs, ewhich indicates that the timezone is
correctly specified in the gtfs itself.
agency_id,agency_name,agency_url,agency_timezone,agency_lang,agency_phone
BKK,BKK,<http://www.bkk.hu,Europe/Budapest,hu,+36> 1 3 255 255
HEV,MÁV-HÉV,<https://www.mav-hev.hu,Europe/Budapest,hu,+36> 1 511 4040

``` r
r5r::r5r_sitrep()
```

    ## $r5r_package_version
    ## [1] '1.1.0'
    ## 
    ## $r5_jar_version
    ## [1] "6.9"
    ## 
    ## $java_version
    ## [1] "11.0.19"
    ## 
    ## $set_memory
    ## [1] "-Xmx8g"
    ## 
    ## $session_info
    ## R version 4.2.2 (2022-10-31 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 22621)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=C                             
    ## [3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] osmdata_0.2.0    osmextract_0.5.0 sf_1.0-8         ggplot2_3.3.6    dplyr_1.1.2      rJava_1.0-6     
    ## [7] r5r_1.1.0       
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.9         pillar_1.9.0       compiler_4.2.2     class_7.3-20       tools_4.2.2       
    ##  [6] digest_0.6.29      checkmate_2.1.0    evaluate_0.17      lifecycle_1.0.3    tibble_3.2.1      
    ## [11] gtable_0.3.1       pkgconfig_2.0.3    rlang_1.1.1        cli_3.4.1          DBI_1.1.3         
    ## [16] rstudioapi_0.14    yaml_2.3.5         xfun_0.33          fastmap_1.1.0      e1071_1.7-11      
    ## [21] withr_2.5.0        stringr_1.5.1      knitr_1.40         sfheaders_0.4.0    generics_0.1.3    
    ## [26] vctrs_0.6.2        classInt_0.4-8     grid_4.2.2         tidyselect_1.2.0   glue_1.6.2        
    ## [31] data.table_1.14.4  R6_2.5.1           fansi_1.0.3        rmarkdown_2.17     magrittr_2.0.3    
    ## [36] backports_1.4.1    units_0.8-0        scales_1.2.1       htmltools_0.5.3    colorspace_2.0-3  
    ## [41] KernSmooth_2.23-20 utf8_1.2.2         proxy_0.4-27       stringi_1.7.8      munsell_0.5.0

Can you please help with the issue or guide me in the right dierection
how to resolve this? Thanks for any help in advance.
