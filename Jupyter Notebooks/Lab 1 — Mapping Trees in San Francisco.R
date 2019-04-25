# Lab 1 — Mapping Trees in San Francisco

# Import data from San Francisco Data Portal
install.packages("bigrquery")
library(bigrquery)
library(dplyr)
library(sf)


billing = "sanguine-parsec-238723" # enter your unique project ID here

sql1 = "
        SELECT * 
        FROM `bigquery-public-data.san_francisco.street_trees`

        "

trees_all = bq_project_query(billing, sql1) %>%
        bq_table_download(quiet = TRUE)

sql2 = "
        SELECT tree_id, longitude, latitude
        FROM `bigquery-public-data.san_francisco.street_trees`  
        WHERE location != '' AND longitude >= -123 AND latitude >= 37.6
        "

trees_tidy = bq_project_query(billing, sql2) %>%
        bq_table_download(quiet = TRUE)

trees_tidy_sf = st_as_sf(trees_tidy, coords = c("longitude", "latitude"), crs = 4326) %>%
        st_transform(crs = 7131)

trees_tidy_kde = trees_tidy_sf %>%
        as('Spatial') %>%
        sp.kde(bw = 400, n = 750, standardize = FALSE)

