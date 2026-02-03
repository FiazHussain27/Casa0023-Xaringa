library(tidyverse)
library(terra)
library(fs)
library(sf)
library(here)

# List your raster files excluding band 8 using the patter argument
listlandsat_8<-dir_info("C:/Users/fiazg/Documents/CASA/Remote Sensing/prac3/Lsat8")%>%
  dplyr::filter(str_detect(path, "[B123456790].TIF")) %>%
  dplyr::select(path)%>%
  pull()%>%
  as.character()%>%
  # Load our raster layers into a stack
  terra::rast()

#For Landsat 9
# List your raster files excluding band 8 using the patter argument
listlandsat_9<-dir_info("C:/Users/fiazg/Documents/CASA/Remote Sensing/prac3/Lsat9")%>%
  dplyr::filter(str_detect(path, "[1B23456790].TIF")) %>%
  dplyr::select(path)%>%
  pull()%>%
  as.character()%>%
  # Load our raster layers into a stack
  terra::rast()
m1 <- terra::mosaic(listlandsat_8, listlandsat_9, fun="mean")

# study area
study_area <- st_read("C:/Users/fiazg/Documents/CASA/Remote Sensing/prac3/Study_area.shp")%>%
  st_transform(., 32634)

m1_clip<-m1%>%
  terra::crop(., study_area)%>%
  terra::mask(., study_area)
# NDVI comparison

m1_NDVI <- (m1_clip$LC08_L2SP_175083_20220501_20220504_02_T1_SR_B5 - m1_clip$LC08_L2SP_175083_20220501_20220504_02_T1_SR_B4 ) / (m1_clip$LC08_L2SP_175083_20220501_20220504_02_T1_SR_B5 + m1_clip$LC08_L2SP_175083_20220501_20220504_02_T1_SR_B4)

m1_NDVI %>%
  plot(.)

veg <- m1_NDVI %>%
  # cbind = combine dataframes, or here or values listed
  terra::classify(., cbind(-Inf, 0.2, NA))

veg %>%
  plot(.)
veg <- m1_NDVI %>%
  # cbind = combine dataframes, or here or values listed
  terra::classify(., cbind(-Inf, 0.2, NA))

veg %>%
  plot(.)
