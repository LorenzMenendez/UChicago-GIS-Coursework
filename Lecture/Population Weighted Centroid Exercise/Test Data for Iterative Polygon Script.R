# Goal: This script will import an adquate test data and official results for us to test on Iterative Polygan Population Weighted Centroid Function

# Future Improvements: Source from IPUMS API instead of downloaded CSVs and SHPs

# Libraries
library(sf)
library(dplyr)


# STEP 1 — Importing Stable and Clean Test Data ---------------------------

# Importing US County Boundaries, then joining on GEOID with population csv
usCounty10 = st_read("DATA/county2010/US_county_2010.shp") %>%
        select(GEOID10, NAME10, GISJOIN)

usCountyPop10 = read.csv("DATA/county2010/nhgis0007_ds172_2010_county.csv") %>%
        select(GISJOIN, H7V001) %>%
        rename(POP = H7V001)

usCountyPop10 = left_join(usCounty10, usCountyPop10, by = "GISJOIN")

# Importing US States Bobundary, then joining on GEOID with population csv
usState10 = st_read("DATA/state2010/US_state_2010.shp") %>%
        select(GEOID10, NAME10, GISJOIN)

usStatePop10 = read.csv("DATA/state2010/nhgis0007_ds172_2010_state.csv") %>%
        select(GISJOIN, H7V001) %>%
        rename(POP = H7V001)

usStatePop10 = left_join(usState10, usStatePop10, by = "GISJOIN")


# STEP 2 — Importing Offical Population Weighted Centroids ----------------

# Importing US Counties Census Mean Population Weighted Centroid
usCountyPopCentroid10 = st_read("DATA/county2010/US_county_cenpop_2010.shp")

# Importing US State Mean Population Weighted Centroid
usStatePopCentroid10 = st_read("DATA/state2010/US_state_cenpop_2010.shp")


# STEP 3 — Remove Unnecessary Data from the Global Environment ------------
remove(usCounty10, usState10)

        
