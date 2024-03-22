Mapping
================
Gergely Som
2024-03-21

\#Essentially I was wondering how to create a contour map that is iny
within the extent of Budapest, but have not really managed to figure out
a way to make the maps more aesthetically pleasing.

``` r
#Loading all the packages used in the creation of the maps
library(rJava)
library(r5r)
library(dplyr)
library(ggplot2)
library(sf)
library(osmextract)
library(osmdata)
library(interp)
library(h3jsr)
```

``` r
#Allocating memory for rJava
.jinit(parameters = "-Xmx8g")
```

    ## [1] 0

``` r
# 1) Build transport network
# Replace 'your_data_path' with the path to your data
your_data_path <- "C:/UCL/Dissertation/transit_data"
r5r_core <- setup_r5(data_path = your_data_path)
```

    ## No raster .tif files found. Using elevation = 'NONE'.

    ## Using cached R5 version from C:/Users/somge/AppData/Local/R/win-library/4.3/r5r/jar/r5-v6.9-all.jar

    ## 
    ## Using cached network.dat from C:/UCL/Dissertation/transit_data/network.dat

``` r
#Reading in the base layers for the accessibility maps
budapest_districts <- st_read("Budapest_districts.kml")
```

    ## Reading layer `overpass-turbo.eu export' from data source `C:\UCL\Dissertation\Budapest_districts.kml' using driver `KML'
    ## Simple feature collection with 495 features and 2 fields
    ## Geometry type: GEOMETRY
    ## Dimension:     XY
    ## Bounding box:  xmin: 18.92511 ymin: 47.34969 xmax: 19.33493 ymax: 47.61315
    ## Geodetic CRS:  WGS 84

``` r
budapest_outline <- st_read("C:/UCL/Dissertation/Budapest.kml")
```

    ## Reading layer `Layer #0' from data source `C:\UCL\Dissertation\Budapest.kml' using driver `KML'
    ## Simple feature collection with 2 features and 2 fields
    ## Geometry type: GEOMETRYCOLLECTION
    ## Dimension:     XY
    ## Bounding box:  xmin: 18.92511 ymin: 47.34969 xmax: 19.33493 ymax: 47.61315
    ## Geodetic CRS:  WGS 84

``` r
geometries_extracted <- st_collection_extract(budapest_outline, "POLYGON")

budapest_districts$admin_level <- rep(9, nrow(budapest_districts))

# Filter out only polygon geometries
budapest_districts_poly <- budapest_districts[st_geometry_type(budapest_districts) %in% c("POLYGON", "MULTIPOLYGON"), ]
```

``` r
#Reading in origins and destinations
orig <- st_read('orig.gpkg') |> st_transform(4326) |> st_centroid()
```

    ## Reading layer `grid_hex' from data source `C:\UCL\Dissertation\orig.gpkg' using driver `GPKG'
    ## Simple feature collection with 1137 features and 0 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 794345 ymin: 5251765 xmax: 827345 ymax: 5282364
    ## Projected CRS: WGS 84 / UTM zone 33N

``` r
dest <- st_read('hospitals.gpkg') |> st_transform(4326) |> st_centroid()
```

    ## Reading layer `hospitals' from data source `C:\UCL\Dissertation\hospitals.gpkg' using driver `GPKG'
    ## Simple feature collection with 45 features and 46 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: 18.93114 ymin: 47.42293 xmax: 19.21118 ymax: 47.58066
    ## Geodetic CRS:  WGS 84

``` r
# For origins
orig <- orig %>%
  mutate(id = row_number(), # Create an ID column
         lat = st_coordinates(geom)[,2], # Extract latitude
         lon = st_coordinates(geom)[,1]) %>% # Extract longitude
  select(id, lat, lon) # Keep only required columns

# For destinations
dest <- dest %>%
  mutate(id = row_number(), # Create an ID column
         lat = st_coordinates(geom)[,2], # Extract latitude
         lon = st_coordinates(geom)[,1]) %>% # Extract longitude
  select(id, lat, lon) # Keep only required columns

# Add an 'opportunities' column with a placeholder value (e.g., 1 for each destination)
dest$opportunities <- 1

# Verify the structures
str(orig)
```

    ## Classes 'sf' and 'data.frame':   1137 obs. of  4 variables:
    ##  $ id  : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ lat : num  47.4 47.4 47.4 47.4 47.4 ...
    ##  $ lon : num  18.9 18.9 18.9 18.9 18.9 ...
    ##  $ geom:sfc_POINT of length 1137; first list element:  'XY' num  18.9 47.4
    ##  - attr(*, "sf_column")= chr "geom"
    ##  - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA
    ##   ..- attr(*, "names")= chr [1:3] "id" "lat" "lon"

``` r
str(dest)
```

    ## Classes 'sf' and 'data.frame':   45 obs. of  5 variables:
    ##  $ id           : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ lat          : num  47.5 47.5 47.5 47.5 47.5 ...
    ##  $ lon          : num  19.1 19.1 19 19 19 ...
    ##  $ geom         :sfc_POINT of length 45; first list element:  'XY' num  19.1 47.5
    ##  $ opportunities: num  1 1 1 1 1 1 1 1 1 1 ...
    ##  - attr(*, "sf_column")= chr "geom"
    ##  - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA
    ##   ..- attr(*, "names")= chr [1:4] "id" "lat" "lon" "opportunities"

``` r
# Running the accessibility() function for daytime accessibility
access_results <- accessibility(
  r5r_core = r5r_core,
  origins = orig,
  destinations = dest,
  opportunities_colnames = "opportunities",
  mode = "TRANSIT",
  departure_datetime = as.POSIXct("2024-03-05 12:00:00"),
  decay_function = "step",
  cutoffs = 30,
  max_trip_duration = 60,
  verbose = TRUE
)
```

    ## Warning in assign_points_input(origins, "origins"): 'origins$id' forcefully cast to character.

    ## Warning in assign_points_input(destinations, "destinations"): 'destinations$id' forcefully cast to character.

``` r
# Assuming 'orig' contains 'lat' and 'lon' columns
# Choose an appropriate resolution level (0 to 15)
res <- 8  # Example resolution level

orig_all_res <- point_to_cell(orig, res = seq(0, 15), simple = FALSE)

# Verify the H3 indexes
head(orig_all_res)
```

    ##   id      lat      lon h3_resolution_0 h3_resolution_1 h3_resolution_2 h3_resolution_3 h3_resolution_4
    ## 1  1 47.36575 18.90521 801ffffffffffff 811e3ffffffffff 821e1ffffffffff 831e03fffffffff 841e1cbffffffff
    ## 2  2 47.38130 18.90636 801ffffffffffff 811e3ffffffffff 821e1ffffffffff 831e03fffffffff 841e1cbffffffff
    ## 3  3 47.39685 18.90751 801ffffffffffff 811e3ffffffffff 821e1ffffffffff 831e03fffffffff 841e1cbffffffff
    ## 4  4 47.41240 18.90866 801ffffffffffff 811e3ffffffffff 821e1ffffffffff 831e03fffffffff 841e1cbffffffff
    ## 5  5 47.42795 18.90981 801ffffffffffff 811e3ffffffffff 821e1ffffffffff 831e03fffffffff 841e037ffffffff
    ## 6  6 47.44350 18.91097 801ffffffffffff 811e3ffffffffff 821e07fffffffff 831e03fffffffff 841e037ffffffff
    ##   h3_resolution_5 h3_resolution_6 h3_resolution_7 h3_resolution_8 h3_resolution_9 h3_resolution_10 h3_resolution_11
    ## 1 851e1cb7fffffff 861e1cb47ffffff 871e1cb45ffffff 881e1cb451fffff 891e1cb451bffff  8a1e1cb45187fff  8b1e1cb45183fff
    ## 2 851e1cb7fffffff 861e1cb67ffffff 871e1cb63ffffff 881e1cb633fffff 891e1cb6323ffff  8a1e1cb6320ffff  8b1e1cb6320afff
    ## 3 851e1cb7fffffff 861e1cb67ffffff 871e1cb61ffffff 881e1cb617fffff 891e1cb617bffff  8a1e1cb617a7fff  8b1e1cb617a5fff
    ## 4 851e1cb7fffffff 861e1cb67ffffff 871e1cb61ffffff 881e1cb619fffff 891e1cb6183ffff  8a1e1cb6182ffff  8b1e1cb6182cfff
    ## 5 851e1cb7fffffff 861e03797ffffff 871e1cb65ffffff 881e1cb65bfffff 891e1cb65a7ffff  8a1e1cb65a6ffff  8b1e1cb65a69fff
    ## 6 851e037bfffffff 861e03797ffffff 871e03796ffffff 881e037961fffff 891e0379617ffff  8a1e03796177fff  8b1e03796170fff
    ##   h3_resolution_12 h3_resolution_13 h3_resolution_14 h3_resolution_15
    ## 1  8c1e1cb451839ff  8d1e1cb4518393f  8e1e1cb45183917  8f1e1cb45183913
    ## 2  8c1e1cb6320a9ff  8d1e1cb6320a83f  8e1e1cb6320a837  8f1e1cb6320a831
    ## 3  8c1e1cb617a59ff  8d1e1cb617a583f  8e1e1cb617a58e7  8f1e1cb617a5819
    ## 4  8c1e1cb6182c9ff  8d1e1cb6182c8ff  8e1e1cb6182c8ef  8f1e1cb6182c8ea
    ## 5  8c1e1cb65a69bff  8d1e1cb65a69bbf  8e1e1cb65a69b97  8f1e1cb65a69b90
    ## 6  8c1e03796170bff  8d1e03796170abf  8e1e03796170aa7  8f1e03796170aa6

``` r
str(orig_all_res)
```

    ## 'data.frame':    1137 obs. of  19 variables:
    ##  $ id              : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ lat             : num  47.4 47.4 47.4 47.4 47.4 ...
    ##  $ lon             : num  18.9 18.9 18.9 18.9 18.9 ...
    ##  $ h3_resolution_0 : chr  "801ffffffffffff" "801ffffffffffff" "801ffffffffffff" "801ffffffffffff" ...
    ##  $ h3_resolution_1 : chr  "811e3ffffffffff" "811e3ffffffffff" "811e3ffffffffff" "811e3ffffffffff" ...
    ##  $ h3_resolution_2 : chr  "821e1ffffffffff" "821e1ffffffffff" "821e1ffffffffff" "821e1ffffffffff" ...
    ##  $ h3_resolution_3 : chr  "831e03fffffffff" "831e03fffffffff" "831e03fffffffff" "831e03fffffffff" ...
    ##  $ h3_resolution_4 : chr  "841e1cbffffffff" "841e1cbffffffff" "841e1cbffffffff" "841e1cbffffffff" ...
    ##  $ h3_resolution_5 : chr  "851e1cb7fffffff" "851e1cb7fffffff" "851e1cb7fffffff" "851e1cb7fffffff" ...
    ##  $ h3_resolution_6 : chr  "861e1cb47ffffff" "861e1cb67ffffff" "861e1cb67ffffff" "861e1cb67ffffff" ...
    ##  $ h3_resolution_7 : chr  "871e1cb45ffffff" "871e1cb63ffffff" "871e1cb61ffffff" "871e1cb61ffffff" ...
    ##  $ h3_resolution_8 : chr  "881e1cb451fffff" "881e1cb633fffff" "881e1cb617fffff" "881e1cb619fffff" ...
    ##  $ h3_resolution_9 : chr  "891e1cb451bffff" "891e1cb6323ffff" "891e1cb617bffff" "891e1cb6183ffff" ...
    ##  $ h3_resolution_10: chr  "8a1e1cb45187fff" "8a1e1cb6320ffff" "8a1e1cb617a7fff" "8a1e1cb6182ffff" ...
    ##  $ h3_resolution_11: chr  "8b1e1cb45183fff" "8b1e1cb6320afff" "8b1e1cb617a5fff" "8b1e1cb6182cfff" ...
    ##  $ h3_resolution_12: chr  "8c1e1cb451839ff" "8c1e1cb6320a9ff" "8c1e1cb617a59ff" "8c1e1cb6182c9ff" ...
    ##  $ h3_resolution_13: chr  "8d1e1cb4518393f" "8d1e1cb6320a83f" "8d1e1cb617a583f" "8d1e1cb6182c8ff" ...
    ##  $ h3_resolution_14: chr  "8e1e1cb45183917" "8e1e1cb6320a837" "8e1e1cb617a58e7" "8e1e1cb6182c8ef" ...
    ##  $ h3_resolution_15: chr  "8f1e1cb45183913" "8f1e1cb6320a831" "8f1e1cb617a5819" "8f1e1cb6182c8ea" ...

``` r
# retrieve polygons of H3 spatial grid
grid <- h3jsr::cell_to_polygon(orig_all_res$h3_resolution_8, simple = FALSE)

str(grid)
```

    ## Classes 'sf' and 'data.frame':   1137 obs. of  2 variables:
    ##  $ h3_address: chr  "881e1cb451fffff" "881e1cb633fffff" "881e1cb617fffff" "881e1cb619fffff" ...
    ##  $ geometry  :sfc_POLYGON of length 1137; first list element: List of 1
    ##   ..$ : num [1:7, 1:2] 18.9 18.9 18.9 18.9 18.9 ...
    ##   ..- attr(*, "class")= chr [1:3] "XY" "POLYGON" "sfg"
    ##  - attr(*, "sf_column")= chr "geometry"
    ##  - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA
    ##   ..- attr(*, "names")= chr "h3_address"

``` r
str(access_results)
```

    ## Classes 'data.table' and 'data.frame':   1137 obs. of  5 variables:
    ##  $ id           : chr  "1" "2" "3" "4" ...
    ##  $ opportunity  : chr  "opportunities" "opportunities" "opportunities" "opportunities" ...
    ##  $ percentile   : int  50 50 50 50 50 50 50 50 50 50 ...
    ##  $ cutoff       : int  30 30 30 30 30 30 30 30 30 30 ...
    ##  $ accessibility: num  0 0 0 0 0 0 0 0 0 1 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
# Add the 'id' from 'orig_all_res' to 'grid'
grid_with_id <- left_join(grid, orig_all_res[, c("h3_resolution_8", "id")], by = c("h3_address" = "h3_resolution_8"))

grid_with_id$id <- as.character(grid_with_id$id)
orig$id <- as.character(orig$id)

# merge accessibility estimates
access_sf <- left_join(grid_with_id, access_results, by = "id")
access_sf
```

    ## Simple feature collection with 1137 features and 6 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 18.8957 ymin: 47.33641 xmax: 19.3574 ymax: 47.6284
    ## Geodetic CRS:  WGS 84
    ## First 10 features:
    ##         h3_address id   opportunity percentile cutoff accessibility                       geometry
    ## 1  881e1cb451fffff  1 opportunities         50     30             0 POLYGON ((18.905 47.36948, ...
    ## 2  881e1cb633fffff  2 opportunities         50     30             0 POLYGON ((18.90029 47.38537...
    ## 3  881e1cb617fffff  3 opportunities         50     30             0 POLYGON ((18.90754 47.39874...
    ## 4  881e1cb619fffff  4 opportunities         50     30             0 POLYGON ((18.90283 47.41462...
    ## 5  881e1cb65bfffff  5 opportunities         50     30             0 POLYGON ((18.89812 47.4305,...
    ## 6  881e037961fffff  6 opportunities         50     30             0 POLYGON ((18.90537 47.44386...
    ## 7  881e03794dfffff  7 opportunities         50     30             0 POLYGON ((18.91027 47.46516...
    ## 8  881e037b21fffff  8 opportunities         50     30             0 POLYGON ((18.90555 47.48103...
    ## 9  881e037b05fffff  9 opportunities         50     30             0 POLYGON ((18.91281 47.49439...
    ## 10 881e037b47fffff 10 opportunities         50     30             1 POLYGON ((18.9081 47.51025,...

``` r
#Clip access_sf with geometries_extracted
clipped_access_sf <- st_intersection(access_sf, geometries_extracted)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all geometries

``` r
orig_df <- as.data.frame(orig)
access_hospital <- as.data.frame(clipped_access_sf)


# interpolate estimates to get spatially smooth result
access_hospitals <- access_hospital %>% 
  filter(opportunity == "opportunities") %>%
  inner_join(orig_df, by='id') %>%
  with(interp::interp(lon, lat, accessibility, duplicate = "strip")) %>%
  with(cbind(acc=as.vector(z),  # Column-major order
             x=rep(x, times=length(y)),
             y=rep(y, each=length(x)))) %>% as.data.frame() %>% na.omit() %>%
  mutate(opportunity = "opportunities")

#Inspect and save the results
str(access_hospitals)
```

    ## 'data.frame':    1091 obs. of  4 variables:
    ##  $ acc        : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ x          : num  19.1 19.1 19.1 19.1 19.1 ...
    ##  $ y          : num  47.4 47.4 47.4 47.4 47.4 ...
    ##  $ opportunity: chr  "opportunities" "opportunities" "opportunities" "opportunities" ...
    ##  - attr(*, "na.action")= 'omit' Named int [1:509] 1 2 3 4 5 6 7 8 9 10 ...
    ##   ..- attr(*, "names")= chr [1:509] "1" "2" "3" "4" ...

``` r
write.csv(access_hospitals, "access_hospitals.csv", row.names = FALSE)
```

``` r
# Assuming budapest_districts_poly is an sf object
bbox <- st_bbox(budapest_districts_poly)
bb_x <- c(bbox["xmin"], bbox["xmax"])
bb_y <- c(bbox["ymin"], bbox["ymax"])
```

``` r
#And now doing the plotting
plot_day <- ggplot(na.omit(access_hospitals)) +
  geom_sf(data = budapest_districts_poly, fill = "lightgray", color = "black") +
  geom_contour_filled(aes(x=x, y=y, z=acc), alpha=.7, breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 15, 18, 21, 24)) +
  scale_fill_viridis_d(direction = -1, option = 'B') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_sf(xlim = bb_x, ylim = bb_y, datum = NA) + 
  labs(fill = "Number of\nfacilities within\n30 minutes") +
  theme_void() +
  facet_wrap(~opportunity)

plot_day
```

![](Plotting_files/figure-gfm/unnamed-chunk-20-1.png)<!-- --> As we can
see the accessibility scores are not confined within the extend of
geometries extracted which is the outline of Budapest.The question would
be is there any way to fill the grey areas that the accessibility score
layer does not cover and also to crop the accessibility layer with the
Budapest outline.
