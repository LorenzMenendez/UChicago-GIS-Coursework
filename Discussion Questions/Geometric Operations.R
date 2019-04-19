# Week 3 Discussion Questions
library(sf)
library(dplyr)
library(spData)

# Question 1

# Rotation function
rotation = function(a){
        r = a * pi / 180 #degrees to radians
        matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
}

# Finding Centroid of the World

world_centroid = st_transform(world, 4088) %>% # Centroid Calculation requires projected CRS
        st_combine() %>%
        st_centroid()

# Applying Rotation
world_rotate = (st_geometry(world)  # Transformation "sf" to "sfc"
         - world_centroid) * rotation(180) + world_centroid

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
        






        
        