Lab 1
================
Óscar Agüero Rodriguez
30/8/2020

  - [Lab 1.3](#lab-1.3)
      - [1.3.1 Read the shapefile storms\_xyz\_feature from the shape
        directory in the sf
        package](#read-the-shapefile-storms_xyz_feature-from-the-shape-directory-in-the-sf-package)
      - [1.3.2 Copy this file to another directory on your computer, and
        read it from there (note: a shapefile consists of more than one
        file\!)](#copy-this-file-to-another-directory-on-your-computer-and-read-it-from-there-note-a-shapefile-consists-of-more-than-one-file)
      - [1.3.3 How many features does this dataset
        contain?](#how-many-features-does-this-dataset-contain)
      - [1.3.4 Plot the dataset, with axes = TRUE (hint: before
        plotting, pipe through st\_zm to drop Z and M coordinates; more
        about this in chapter
        3).](#plot-the-dataset-with-axes-true-hint-before-plotting-pipe-through-st_zm-to-drop-z-and-m-coordinates-more-about-this-in-chapter-3.)
      - [1.3.5 Before plotting, pipe the dataset through
        st\_set\_crs(4326). What is different in the plot
        obtained?](#before-plotting-pipe-the-dataset-through-st_set_crs4326.-what-is-different-in-the-plot-obtained)

Paquetes a utilizar

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.1
    ## v tidyr   1.1.1     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(sf)
```

    ## Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1

``` r
library(mapview)
library(abind)
library(stars)
library(RSQLite)
library(spacetime)
library(xts)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

``` r
library(starsdata)
library(spData)
library(spDataLarge)
library(units)
```

    ## udunits system database from C:/Users/Oscar/Documents/R/win-library/4.0/units/share/udunits

``` r
library(cubelyr)
```

# Lab 1.3

## 1.3.1 Read the shapefile storms\_xyz\_feature from the shape directory in the sf package

``` r
tst = st_read(system.file("shape/storms_xyz_feature.shp", package="sf"), quiet = TRUE)
class(st_geometry(tst))
```

    ## [1] "sfc_LINESTRING" "sfc"

``` r
class(st_geometry(tst)[[1]])
```

    ## [1] "XYZ"        "LINESTRING" "sfg"

## 1.3.2 Copy this file to another directory on your computer, and read it from there (note: a shapefile consists of more than one file\!)

``` r
setwd("~/Estadística/Spacial Stats/Storms_xyz")
tst1 <- st_read("storms_xyz_feature.shp")
```

    ## Reading layer `storms_xyz_feature' from data source `C:\Users\Oscar\Documents\EstadÃ­stica\Spacial Stats\Storms_xyz\storms_xyz_feature.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 71 features and 1 field
    ## geometry type:  LINESTRING
    ## dimension:      XYZ
    ## bbox:           xmin: -102.2 ymin: 8.3 xmax: 0 ymax: 59.5
    ## z_range:        zmin: 924 zmax: 1017
    ## CRS:            NA

## 1.3.3 How many features does this dataset contain?

``` r
tst1
```

    ## Simple feature collection with 71 features and 1 field
    ## geometry type:  LINESTRING
    ## dimension:      XYZ
    ## bbox:           xmin: -102.2 ymin: 8.3 xmax: 0 ymax: 59.5
    ## z_range:        zmin: 924 zmax: 1017
    ## CRS:            NA
    ## First 10 features:
    ##      Track                       geometry
    ## 1     TONY LINESTRING Z (-50.8 20.1 10...
    ## 2    SANDY LINESTRING Z (-77.4 14.3 10...
    ## 3   RAFAEL LINESTRING Z (-62.7 14.7 10...
    ## 4    PATTY LINESTRING Z (-72.5 25.5 10...
    ## 5    OSCAR LINESTRING Z (-38 12.4 1008...
    ## 6   NADINE LINESTRING Z (-38 15.5 1008...
    ## 7  MICHAEL LINESTRING Z (-36.7 28.9 10...
    ## 8   LESLIE LINESTRING Z (-27.4 12.9 10...
    ## 9     KIRK LINESTRING Z (-43.4 23.9 10...
    ## 10   JOYCE LINESTRING Z (-31.7 10.7 10...

Tiene 71 features

## 1.3.4 Plot the dataset, with axes = TRUE (hint: before plotting, pipe through st\_zm to drop Z and M coordinates; more about this in chapter 3).

``` r
system.file("shape/storms_xyz_feature.shp", package="sf") %>%
  read_sf() %>%
  st_zm() %>% 
  plot(graticule = TRUE, axes = TRUE)
```

![](Lab-1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## 1.3.5 Before plotting, pipe the dataset through st\_set\_crs(4326). What is different in the plot obtained?

``` r
system.file("shape/storms_xyz_feature.shp", package="sf") %>%
  read_sf() %>%
  st_zm() %>% 
  st_set_crs(4326) %>% 
  plot(graticule = TRUE, axes = TRUE)
```

![](Lab-1_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
