

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
source('~/Google Drive/2018-2019 Homework/Spring Quarter/GEOG 28602/Lecture/Population Weighted Centroid Exercise/Test Data for Iterative Polygon Script.R', echo=TRUE)
bigAs = usStatePop10
litAs = usCountyPop10

#0 Init final base R data frame. This will get filled as the algortihm does through iterations 
wgtdCentroids = data.frame("X" = numeric(), "Y" = numeric(), "POP" = integer())

#1 Iteration over rows of Big Areas 'bigAs'
for (row in 1:nrow(bigAs)) {
        bigA = bigAs[row,]
        
#2. Select/Confirm litA within bigA
        litA = st_join(bigA, litAs, join = st_contains)
        
#3. Return a data.frame of normal centroid coordiantes in 'X' 'Y' columns, with corresponding population in 'POP' column
        subACentroid = st_centroid(litA) %>%
                st_coordinates() %>%
                as.data.frame() %>%
                cbind(as.data.frame(litA$POP.y)) %>%
                rename(POP = `litA$POP.y`)
        
#4. Calculate Weighted Mean Centroid
        # wi * xi, wi * yi
        subACentroid$xWgtd = with(subACentroid, X * POP)
        subACentroid$yWgtd = with(subACentroid, Y * POP)
        
        # X, Y, bigPOP
        bigPOP = sum(subACentroid$POP)
        bigX = sum(subACentroid$xWgtd) / bigPOP
        bigY = sum(subACentroid$yWgtd) / bigPOP
        
        
        # Creating a data.frame with X,Y and POP columns
        bigXY = data.frame("X" = bigX, "Y" = bigY, "POP" = bigPOP)
        
        # Appending 'bigXY' to initialized data.frame 'wgtdCentroids'
        wgtdCentroids = rbind(wgtdCentroids, bigXY)
        
        
}

# 5. Converting final data.frame to 'sf'object
wgtdCentroids_sf = na.omit(wgtdCentroids) %>% # Will have to figure out how to handle NAs
        st_as_sf(coords = c("X", "Y"), crs = st_crs(bigAs))
        
        
        
        