# Chapter 13 — Geomarketing, Bike Shops in Germany
library(sf)
library(dplyr)
library(purrr)
library(raster)
library(osmdata)
library(spDataLarge)


# 13.3 Tidy the Input Data ------------------------------------------------
download.file("https://tinyurl.com/ybtpkwxz", 
              destfile = "census.zip", mode = "wb")
unzip("census.zip") # unzip the files
census_de = readr::read_csv2(list.files(pattern = "Gitter.csv"))

# pop = population, hh_size = household size
input = dplyr::select(census_de, x = x_mp_1km, y = y_mp_1km, pop = Einwohner,
                      women = Frauen_A, mean_age = Alter_D,
                      hh_size = HHGroesse_D)
# set -1 and -9 to NA
input_tidy = mutate_all(input, list(~ifelse(. %in% c(-1, -9), NA, .)))


# 13.4 Create census rasters ----------------------------------------------
input_ras = rasterFromXYZ(input_tidy, crs = st_crs(3035)$proj4string)

# Creating new classifications
rcl_pop = matrix(c(1, 1, 127, 2, 2, 375, 3, 3, 1250, 
                   4, 4, 3000, 5, 5, 6000, 6, 6, 8000), 
                 ncol = 3, byrow = TRUE)
rcl_women = matrix(c(1, 1, 3, 2, 2, 2, 3, 3, 1, 4, 5, 0), 
                   ncol = 3, byrow = TRUE)
rcl_age = matrix(c(1, 1, 3, 2, 2, 0, 3, 5, 0),
                 ncol = 3, byrow = TRUE)
rcl_hh = rcl_women
rcl = list(rcl_pop, rcl_women, rcl_age, rcl_hh)

# Reclassification loop
reclass = input_ras
for (i in seq_len(nlayers(reclass))) {
        reclass[[i]] = reclassify(x = reclass[[i]], rcl = rcl[[i]], right = NA)
}
names(reclass) = names(input_ras)


# 13.5 Define metropolitan areas ------------------------------------------
pop_agg = aggregate(reclass$pop, fact = 20, fun = sum) # Lower the raster resolution
pop_agg = pop_agg[pop_agg > 500000, drop = FALSE] # Only keep raster cells with high pop

# Combining city raster into polygons
polys = pop_agg %>% 
        clump() %>%
        rasterToPolygons() %>%
        st_as_sf()

metros = polys %>%
        group_by(clumps) %>%
        summarize()

# Geocoding the results to get city names 
metros_wgs = st_transform(metros, 4326)
coords = st_centroid(metros_wgs) %>%
        st_coordinates() %>%
        round(4)

library(revgeo)
metro_names = revgeo(longitude = coords[, 1], latitude = coords[, 2], 
                     output = "frame")

metro_names = dplyr::pull(metro_names, city) %>% # One city did not work, so a quick workaround
        as.character() %>% 
        ifelse(. == "Wülfrath", "Duesseldorf", .)


# 13.6 Points of Interest -------------------------------------------------
shops = map(metro_names, function(x) {
        message("Downloading shops of: ", x, "\n")
        # give the server a bit time
        Sys.sleep(sample(seq(5, 10, 0.1), 1))
        query = opq(x) %>%
                add_osm_feature(key = "shop")
        points = osmdata_sf(query)
        # request the same data again if nothing has been downloaded
        iter = 2
        while (nrow(points$osm_points) == 0 & iter > 0) {
                points = osmdata_sf(query)
                iter = iter - 1
        }
        points = st_set_crs(points$osm_points, 4326)
})

# checking if we have downloaded shops for each metropolitan area
ind = map(shops, nrow) == 0
if (any(ind)) {
        message("There are/is still (a) metropolitan area/s without any features:\n",
                paste(metro_names[ind], collapse = ", "), "\nPlease fix it!")
}

# select only specific columns
shops = map(shops, dplyr::select, osm_id, shop)

# putting all list elements into a single data frame
shops = do.call(rbind, shops)

shops = st_transform(shops, proj4string(reclass))
# create poi raster
poi = rasterize(x = shops, y = reclass, field = "osm_id", fun = "count")

# construct reclassification matrix
int = classInt::classIntervals(values(poi), n = 4, style = "fisher")
int = round(int$brks)
rcl_poi = matrix(c(int[1], rep(int[-c(1, length(int))], each = 2), 
                   int[length(int)] + 1), ncol = 2, byrow = TRUE)
rcl_poi = cbind(rcl_poi, 0:3)  

# reclassify
poi = reclassify(poi, rcl = rcl_poi, right = NA) 
names(poi) = "poi"


# 13.7 Identifying suitable locations -------------------------------------

# add poi raster
reclass = addLayer(reclass, poi)
# delete population raster
reclass = dropLayer(reclass, "pop")

# calculate the total score
result = sum(reclass)
