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
library(rJava)
library(dplyr)
library(ggplot2)
library(sf)
library(osmextract)
library(osmdata)
library(readr)
```

\#Build Transport Network We specify the data path and initialize the R5
transport network.

``` r
options(java.parameters = "-Xmx8g")

.jinit()
```

    ## [1] 0

``` r
# Specify the data path
your_data_path <- "C:/UCL/Dissertation/transit_data"

list.files("C:/UCL/Dissertation/transit_data")
```

    ## [1] "budapest_gtfs (3).zip"          "calendar.txt"                   "hungary-latest.gpkg"           
    ## [4] "hungary-latest.osm.pbf"         "hungary-latest.osm.pbf.mapdb"   "hungary-latest.osm.pbf.mapdb.p"
    ## [7] "network.dat"                    "network_settings.json"

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
    ## Running under: Windows 10 x64 (build 22631)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] readr_2.1.3      osmdata_0.2.0    osmextract_0.5.0 ggplot2_3.3.6    dplyr_1.1.2      sf_1.0-8        
    ## [7] r5r_1.1.0        rJava_1.0-6     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.0   xfun_0.33          colorspace_2.0-3   vctrs_0.6.2        generics_0.1.3    
    ##  [6] htmltools_0.5.3    yaml_2.3.5         utf8_1.2.2         rlang_1.1.1        e1071_1.7-11      
    ## [11] pillar_1.9.0       glue_1.6.2         withr_2.5.0        DBI_1.1.3          bit64_4.0.5       
    ## [16] lifecycle_1.0.3    stringr_1.5.1      munsell_0.5.0      gtable_0.3.1       evaluate_0.17     
    ## [21] knitr_1.40         tzdb_0.3.0         fastmap_1.1.0      parallel_4.2.2     class_7.3-20      
    ## [26] fansi_1.0.3        Rcpp_1.0.9         KernSmooth_2.23-20 backports_1.4.1    scales_1.2.1      
    ## [31] classInt_0.4-8     checkmate_2.1.0    vroom_1.6.0        bit_4.0.4          hms_1.1.2         
    ## [36] digest_0.6.29      stringi_1.7.8      grid_4.2.2         cli_3.4.1          tools_4.2.2       
    ## [41] magrittr_2.0.3     proxy_0.4-27       tibble_3.2.1       crayon_1.5.2       sfheaders_0.4.0   
    ## [46] pkgconfig_2.0.3    ellipsis_0.3.2     data.table_1.14.4  rmarkdown_2.17     rstudioapi_0.14   
    ## [51] R6_2.5.1           units_0.8-0        compiler_4.2.2

Can you please help with the issue or guide me in the right dierection
how to resolve this? Thanks for any help in advance.

``` r
ettm_window <- expanded_travel_time_matrix(r5r_core,   
                                             origins = origin_sf,
                                             destinations = destination_sf,    
                                             mode = c("Transit", "WALK"),
                                             max_trip_duration = 120,
                                             departure_datetime = departure_datetime,
                                             breakdown = TRUE,
                                             time_window = 10)
str(ettm_window)
```

    ## Classes 'data.table' and 'data.frame':   10 obs. of  12 variables:
    ##  $ from_id       : chr  "origin_1" "origin_1" "origin_1" "origin_1" ...
    ##  $ to_id         : chr  "destination_1" "destination_1" "destination_1" "destination_1" ...
    ##  $ departure_time: chr  "12:00:00" "12:01:00" "12:02:00" "12:03:00" ...
    ##  $ draw_number   : int  1 1 1 1 1 1 1 1 1 1
    ##  $ access_time   : num  0 0 0 0 0 0 0 0 0 0
    ##  $ wait_time     : num  0 0 0 0 0 0 0 0 0 0
    ##  $ ride_time     : num  0 0 0 0 0 0 0 0 0 0
    ##  $ transfer_time : num  0 0 0 0 0 0 0 0 0 0
    ##  $ egress_time   : num  0 0 0 0 0 0 0 0 0 0
    ##  $ routes        : chr  "[WALK]" "[WALK]" "[WALK]" "[WALK]" ...
    ##  $ n_rides       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ total_time    : num  30 30 30 30 30 30 30 30 30 30
    ##  - attr(*, ".internal.selfref")=<externalptr>

And not quite correct but in midday congestion somewhat feasible results
for transport mode “BICYCLE”:

``` r
ettm_window_bicycle <- expanded_travel_time_matrix(r5r_core,   
                                             origins = origin_sf,
                                             destinations = destination_sf,    
                                             mode = c("BICYCLE"),
                                             max_trip_duration = 120,
                                             departure_datetime = departure_datetime,
                                             breakdown = TRUE,
                                             time_window = 10)

str(ettm_window_bicycle)
```

    ## Classes 'data.table' and 'data.frame':   10 obs. of  12 variables:
    ##  $ from_id       : chr  "origin_1" "origin_1" "origin_1" "origin_1" ...
    ##  $ to_id         : chr  "destination_1" "destination_1" "destination_1" "destination_1" ...
    ##  $ departure_time: chr  "12:00:00" "12:01:00" "12:02:00" "12:03:00" ...
    ##  $ draw_number   : int  1 1 1 1 1 1 1 1 1 1
    ##  $ access_time   : num  0 0 0 0 0 0 0 0 0 0
    ##  $ wait_time     : num  0 0 0 0 0 0 0 0 0 0
    ##  $ ride_time     : num  0 0 0 0 0 0 0 0 0 0
    ##  $ transfer_time : num  0 0 0 0 0 0 0 0 0 0
    ##  $ egress_time   : num  0 0 0 0 0 0 0 0 0 0
    ##  $ routes        : chr  "[BICYCLE]" "[BICYCLE]" "[BICYCLE]" "[BICYCLE]" ...
    ##  $ n_rides       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ total_time    : num  17 17 17 17 17 17 17 17 17 17
    ##  - attr(*, ".internal.selfref")=<externalptr>

Although it does return correct results for transport mode “CAR”:

``` r
ettm_window_car <- expanded_travel_time_matrix(r5r_core,   
                                             origins = origin_sf,
                                             destinations = destination_sf,    
                                             mode = c("CAR"),
                                             max_trip_duration = 120,
                                             departure_datetime = departure_datetime,
                                             breakdown = TRUE,
                                             time_window = 10)

str(ettm_window_car)
```

    ## Classes 'data.table' and 'data.frame':   10 obs. of  12 variables:
    ##  $ from_id       : chr  "origin_1" "origin_1" "origin_1" "origin_1" ...
    ##  $ to_id         : chr  "destination_1" "destination_1" "destination_1" "destination_1" ...
    ##  $ departure_time: chr  "12:00:00" "12:01:00" "12:02:00" "12:03:00" ...
    ##  $ draw_number   : int  1 1 1 1 1 1 1 1 1 1
    ##  $ access_time   : num  0 0 0 0 0 0 0 0 0 0
    ##  $ wait_time     : num  0 0 0 0 0 0 0 0 0 0
    ##  $ ride_time     : num  0 0 0 0 0 0 0 0 0 0
    ##  $ transfer_time : num  0 0 0 0 0 0 0 0 0 0
    ##  $ egress_time   : num  0 0 0 0 0 0 0 0 0 0
    ##  $ routes        : chr  "[CAR]" "[CAR]" "[CAR]" "[CAR]" ...
    ##  $ n_rides       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ total_time    : num  6 6 6 6 6 6 6 6 6 6
    ##  - attr(*, ".internal.selfref")=<externalptr>

I have created the calendar.txt from calendar_dates.txt the following
way.

``` r
# Load calendar_dates.txt
calendar_dates <- read_csv("C:/UCL/Dissertation/calendar_dates.csv")
```

    ## Rows: 8271 Columns: 3
    ## ── Column specification ─────────────────────────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): service_id
    ## dbl (2): date, exception_type
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Aggregate to find the minimum and maximum dates for each service_id
service_periods <- calendar_dates %>%
  group_by(service_id) %>%
  summarize(start_date = min(date), end_date = max(date))

# Assuming services run every day, create a calendar dataframe
calendar <- service_periods %>%
  mutate(monday = 1,
         tuesday = 1,
         wednesday = 1,
         thursday = 1,
         friday = 1,
         saturday = 1,
         sunday = 1,
         start_date = as.character(start_date),
         end_date = as.character(end_date))

# Select and rename columns to match the GTFS calendar.txt format
calendar <- calendar %>%
  select(service_id, monday, tuesday, wednesday, thursday, friday, saturday, sunday, start_date, end_date)

str(calendar)
```

    ## tibble [1,351 × 10] (S3: tbl_df/tbl/data.frame)
    ##  $ service_id: chr [1:1351] "36134" "36135" "36136" "36137" ...
    ##  $ monday    : num [1:1351] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ tuesday   : num [1:1351] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ wednesday : num [1:1351] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ thursday  : num [1:1351] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ friday    : num [1:1351] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ saturday  : num [1:1351] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ sunday    : num [1:1351] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ start_date: chr [1:1351] "20240302" "20240302" "20240302" "20240302" ...
    ##  $ end_date  : chr [1:1351] "20240402" "20240401" "20240402" "20240401" ...

``` r
# Writing it into a .txt
write.table(calendar, 
            "C:/UCL/Dissertation/transit_data/calendar.txt", 
            sep = ",", 
            row.names = FALSE, 
            col.names = TRUE, 
            quote = FALSE)
```

But then running either detailed_itineraries ro
Expanded_travel_time_matrix still returns the modes as only “walk”.

``` r
# Set the departure time which is clearly within the start and end date of the calendar.txt
departure_datetime <- as.POSIXct("2024-03-05 12:00:00", format = "%Y-%m-%d %H:%M:%S")

# Run the detailed_itineraries function for the specific origin and destination
test_itineraries <- detailed_itineraries(
  r5r_core = r5r_core,
  origins = origin_sf,
  destinations = destination_sf,
  mode = c("TRANSIT", "WALK"),
  departure_datetime = departure_datetime,
  max_trip_duration = 120,  # Ensure this is sufficiently high
  time_window = 30,  # Time window
  suboptimal_minutes = 15,  # Allow consideration of slightly slower routes
  all_to_all = FALSE,  # Since we're testing a single O-D pair
  shortest_path = FALSE,  # Allow suboptimal routes
  drop_geometry = FALSE  # Keep geometry for mapping
)

# Check the structure and modes of the resulting itineraries
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

And it is the same with ettm as well

``` r
ettm_window <- expanded_travel_time_matrix(r5r_core,   
                                           origins = origin_sf,
                                           destinations = destination_sf,    
                                           mode = c("TRAM"),
                                           max_trip_duration = 120,
                                           departure_datetime = departure_datetime,
                                           breakdown = TRUE,
                                           time_window = 10)

str(ettm_window)
```

    ## Classes 'data.table' and 'data.frame':   10 obs. of  12 variables:
    ##  $ from_id       : chr  "origin_1" "origin_1" "origin_1" "origin_1" ...
    ##  $ to_id         : chr  "destination_1" "destination_1" "destination_1" "destination_1" ...
    ##  $ departure_time: chr  "12:00:00" "12:01:00" "12:02:00" "12:03:00" ...
    ##  $ draw_number   : int  1 1 1 1 1 1 1 1 1 1
    ##  $ access_time   : num  0 0 0 0 0 0 0 0 0 0
    ##  $ wait_time     : num  0 0 0 0 0 0 0 0 0 0
    ##  $ ride_time     : num  0 0 0 0 0 0 0 0 0 0
    ##  $ transfer_time : num  0 0 0 0 0 0 0 0 0 0
    ##  $ egress_time   : num  0 0 0 0 0 0 0 0 0 0
    ##  $ routes        : chr  "[WALK]" "[WALK]" "[WALK]" "[WALK]" ...
    ##  $ n_rides       : int  0 0 0 0 0 0 0 0 0 0
    ##  $ total_time    : num  30 30 30 30 30 30 30 30 30 30
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
unique(ettm_window$routes)
```

    ## [1] "[WALK]"
