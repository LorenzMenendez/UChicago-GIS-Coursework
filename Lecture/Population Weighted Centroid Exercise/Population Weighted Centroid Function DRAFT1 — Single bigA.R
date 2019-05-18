# STEP 1: 
#Read Community Area boundaries

# STEP 2:
#Read Census tract boundaries and population data
#Claculate regular centroids for each one

# STEP 3: 
# Take the regular centroid coordinates and scale them based on population using the math. This give the Weighted coordinates
# TIP: Python code uses an interation by row function to go through the larger boundaries. 

#Libraries
library(sf)
library(dplyr)

# Test Data, renamed for generality
source('~/Google Drive/2018-2019 Homework/Spring Quarter/GEOG 28602/Lecture/Population Weighted Centroid Exercise/Test Data Wrangling.R', echo=TRUE)
bigA = ilStatePop10
litA = ilCountyPop10

#1. Return a data.frame of normal centroid coordiantes in 'X' 'Y' columns, with corresponding population in 'POP' column
subACentroid = st_centroid(litA) %>%
        st_coordinates() %>%
        as.data.frame() %>%
        cbind(as.data.frame(litA$POP)) %>%
        rename(POP = `litA$POP`)

#2. Calculate Weighted Mean Centroid
        # wi * xi, wi * yi
        subACentroid$xWgtd = with(subACentroid, X * POP)
        subACentroid$yWgtd = with(subACentroid, Y * POP)

        # X, Y
        bigX = sum(subACentroid$xWgtd) / sum(subACentroid$POP)
        bigY = sum(subACentroid$yWgtd) / sum(subACentroid$POP)
        
        # Tranform bigX, bigY to sf object
        bigXY_sf = c(bigX, bigY) %>%
                st_point(., dim = "XY") %>%
                st_sfc() %>%
                st_set_crs(st_crs(ilState10))
        
        

