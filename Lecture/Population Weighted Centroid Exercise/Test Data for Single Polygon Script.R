# Goal: This script will import an adquate test data and official results for us to test on Single Polygan Population Weighted Centroid Function

# Future Improvements: Source from IPUMS API instead of downloaded CSVs and SHPs

# Libraries
library(sf)
library(dplyr)


# STEP 1 — Importing Stable and Clean Test Data ---------------------------

# Importing IL County Boundaries, then joining on GEOID with population csv
ilCounty10 = st_read("DATA/county2010/US_county_2010.shp") %>%
        filter(STATEFP10 == 17) %>%
        select(GEOID10, NAME10, GISJOIN)

usCountyPop10 = read.csv("DATA/county2010/nhgis0007_ds172_2010_county.csv") %>%
        select(GISJOIN, H7V001) %>%
        rename(POP = H7V001)

ilCountyPop10 = left_join(ilCounty10, usCountyPop10, by = "GISJOIN")

# Importing IL State Bobundary, then joining on GEOID with population csv
ilState10 = st_read("DATA/state2010/US_state_2010.shp") %>%
        filter(STATEFP10 == 17) %>%
        select(GEOID10, NAME10, GISJOIN)

usStatePop10 = read.csv("DATA/state2010/nhgis0007_ds172_2010_state.csv") %>%
        select(GISJOIN, H7V001) %>%
        rename(POP = H7V001)

ilStatePop10 = left_join(ilState10, usStatePop10, by = "GISJOIN")


# STEP 2 — Importing Offical Population Weighted Centroids ----------------

# Importing IL Counties Census Mean Population Weighted Centroid
ilCountiesPopCentroid10 = st_read("DATA/county2010/US_county_cenpop_2010.shp") %>%
        filter(STNAME == "Illinois") %>%

# Importing IL State Mean Population Weighted Centroid
ilStatePopCentroid10 = st_read("DATA/state2010/US_state_cenpop_2010.shp") %>%
        filter(STNAME == "Illinois")


# STEP 3 — Remove Unnecessary Data from the Global Environment ------------
#remove(ilCounty10, ilState10, usCountyPop10, usStatePop10)

        
