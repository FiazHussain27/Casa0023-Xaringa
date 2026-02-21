library(tidyverse)
library(terra)
library(fs)
library(sf)
library(here)
library(terra)
library(RStoolbox)
library(tidyverse)
library(fs)

## Import meta-data and bands based on MTL file
# MUST BE LEVEL 1 (digital number) not LEVEL 2 (surface reflectance) - see more info later.
mtlFile  <- ("C:/Users/fiazg/Documents/CASA/Remote Sensing/prac3/Lsat8/LC08_L2SP_175083_20220501_20220504_02_T1_MTL.txt")
file.exists(mtlFile)

metaData <- readMeta(mtlFile, raw = TRUE)

# 1. Extract band filenames from the metadata
band_names <- metaData$PRODUCT_CONTENTS[
  grep("FILE_NAME_BAND", rownames(metaData$PRODUCT_CONTENTS)),
  "VALUE"
]

# 2. Build full paths to each band
band_paths <- file.path(dirname(mtlFile), band_names)

# 3. Create the raster stack
lsatMeta <- raster::stack(band_paths)

# 4. Check the result
lsatMeta

l8_boa_ref <- lsatMeta

Ok # surface reflectance with DOS

#l8_boa_ref <- radCor(lsatMeta, metaData, method = "dos")

#l8_boa_ref <- radCor(lsatMeta, mtlFile, method = "dos")

#terra::writeRaster(l8_boa_ref, datatype="FLT4S", filename = "prac_3/Lsatdata8/l8_boa_ref.tif", format = "GTiff", overwrite=TRUE)

# Radiance 

#lsat_rad <- radCor(lsatMeta, metaData = metaData, method = "rad")
#hazeDN    <- RStoolbox::estimateHaze(lsat, hazeBands = 2:4, darkProp = 0.01, plot = TRUE)

#lsat_sref <- radCor(lsatMeta, metaData = metaData, method = "dos", 
                    hazeValues = hazeDN, hazeBands = 2:4)

#terra::writeRaster(lsat_rad, datatype="FLT4S", filename = "prac_3/Lsatdata8/lsat_rad.tif", format = "GTiff", overwrite=TRUE)



#List your raster files excluding band 8 using the patter argument

listlandsat_8<-dir_info("C:/Users/fiazg/Documents/CASA/Remote Sensing/prac3/Lsat8")%>%
dplyr::filter(str_detect(path, "[B123456790].TIF")) %>%
dplyr::select(path)%>%
pull()%>%
as.character()%>%
#Load our raster layers into a stack
terra::rast()

#For Landsat 9
#List your raster files excluding band 8 using the patter argument
listlandsat_9<-dir_info("C:/Users/fiazg/Documents/CASA/Remote Sensing/prac3/Lsat9")%>%
dplyr::filter(str_detect(path, "[1B23456790].TIF")) %>%
dplyr::select(path)%>%
pull()%>%
as.character()%>%
  # Load our raster layers into a stack
  terra::rast()


m1 <- terra::mosaic(listlandsat_8, listlandsat_9, fun="mean")

listlandsat_8 <- dir_info("C:/Users/fiazg/Documents/CASA/Remote Sensing/prac3/Lsat8") %>%
  filter(str_detect(path, "SR_B[1-7].TIF$")) %>%
  pull() %>%
  rast()

listlandsat_9 <- dir_info("C:/Users/fiazg/Documents/CASA/Remote Sensing/prac3/Lsat9") %>%
  filter(str_detect(path, "SR_B[1-7].TIF$")) %>%
  pull() %>%
  rast()

m1 <- mosaic(listlandsat_8, listlandsat_9, fun = "mean")

##Enhancement 

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


# for a 3 by 3 filter on 1 band, w means the window and 3 means a 3 by 3 square.

m1_filter <- terra::focal(m1_clip$LC08_L2SP_175083_20220501_20220504_02_T1_SR_B4, w=3)

#texture

library(GLCMTextures)

scale <-(m1_clip*0.0000275) + -0.2

textures1<- glcm_textures(
  scale$LC08_L2SP_175083_20220501_20220504_02_T1_SR_B4,
  # size of window
  w = c(7,7),
  # levels means divide the data into 4 "bins" e.g. a range of 0-20
  # would be 0-5, 5-10, 10-15,15-20
  n_levels = 4,
  # raster data might not be greater than 0
  # convert it to a discrete number of grey levels (e.g. 4)
  # the data is equally divided between the 4 levels by using "range"
  quant_method = "range",
  #co-occurence (second order) matrix (1,0) = one pixel to the right
  # default is all directions as below
  shift = list(c(1, 0), c(1, 1), c(0, 1), c(-1, 1)),
  # select what we want
  metrics="glcm_homogeneity") 

plot(textures1)

# we can just use the c function to combine rasters in terra

raster_and_texture <- c(m1_clip, textures1)
