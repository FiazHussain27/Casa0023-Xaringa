library(tidyverse)
library(sf)
library(tmap)

temp <- st_read("C:/Users/fiazg/Documents/CASA/Remote Sensing/Practical 8/drive-download-20260405T095257Z-1-001/mean_Landsat_level2.shp")

tmap_mode("plot")
# plot each map
tm1 <- tm_shape(temp) + 
  tm_polygons("mean", 
              # breaks=breaks,
              palette="Reds")+
  tm_legend(show=TRUE)+
  tm_layout(frame=FALSE)

tm1
