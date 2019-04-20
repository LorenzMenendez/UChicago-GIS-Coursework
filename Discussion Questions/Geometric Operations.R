# Week 3 Discussion Questions

# Question 1
library(sf)
library(dplyr)
library(spData)

# Rotation function
rotation = function(a){
        r = a * pi / 180 #degrees to radians
        matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
}

# Finding Centroid of the World

world_centroid = st_transform(nz, 4088) %>% # Centroid Calculation requires projected CRS
        st_combine() %>%
        st_centroid()

# Applying Rotation
world_rotate = (st_geometry(nz)  # Transformation "sf" to "sfc"
         - world_centroid) * rotation(180) + world_centroid

# Question 2
library(raster)
library(sf)
library(dplyr)

# Creating the grain raster
grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = raster(nrows = 6, ncols = 6, res = 0.5, 
               xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
               vals = grain_fact)

grain_poly = rasterToPolygons(grain) %>% # Returns a polygonizes grain raster
        st_as_sf()

clay_poly = grain_poly %>% # Returns a tibble/sf multipolygon object where the grain is "clay"
        group_by(layer) %>%
        summarize() %>%
        filter(layer == 1)



# Question 3
library(sf)
library(dplyr)
library(spData)
library(units)

# Returns total Boundary distance
bound_length1 = st_transform(us_states, 2163) %>%
        st_cast(to = "MULTILINESTRING") %>%
        st_length() %>%
        sum() %>%
        set_units(mi)

# Returns boundaries as MULTILINESTRING
bound = st_transform(us_states, 2163) %>%
        st_cast(to = "MULTILINESTRING")

# Returns Length of Boundary for each State in descending order
bound_length = mutate(bound, boundLength = st_length(bound$geometry)) %>%
        arrange(desc(boundLength))

# Knowing descending order,we can have R return Longest and Shortest using head/tail functions
head(bound_length, 1)
tail(bound_length, 1)
        






        
        