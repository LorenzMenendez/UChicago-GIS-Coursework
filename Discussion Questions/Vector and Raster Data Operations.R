# Libraries


# Question 1
library(spData)
library(sf)
library(dplyr)

nz_height_joined = nz %>%
        st_join(nz_height, left = FALSE) %>% # Inner Join on ST_Intersects 
        st_drop_geometry() %>% # Drop Geometry for faster processing
        group_by(Name) %>% # Group by Name of Region
        tally(, sort = TRUE) # Count number of points per region

# Question 2
library(spDataLarge)
library(raster)
library(ggplot2)
library(dplyr)

LandsatRaster = system.file("raster/landsat.tif", package="spDataLarge") %>% # Import Landsat Imagery into R as RasterStack
        stack()

LandsatNDVI = calc(LandsatRaster, fun=function(x) (x[["landsat.4"]] - x[["landsat.3"]])/(x[["landsat.4"]] + x[["landsat.3"]])) # Calculate NDVI raster from LandsatRaster =

plot(LandsatNDVI)

# Question 3
library(raster)
library(ggplot2)

coolRaster = raster(system.file("external/rlogo.grd", package = "raster"))
rlyCoolRaster = focal(coolRaster, w=matrix(.5, nrow=3, ncol=7), fun=max)
plot(rlyCoolRaster)

        
        
        