# Week 2 Workshop — Single-Dataset GIS Operations
library(sf)
library(ggplot2)

# Reading 1992 Ward Shapefiles and plot
ward92 = read_sf("R Spatial Workshop/data/ward1992.shp")

# Reading 1998 Ward Shapefiles and plot
ward98 = read_sf("R Spatial Workshop/data/ward1998.shp")

# Checking projections and reprojections
st_crs(ward98)

st_transform(ward92, 32616) # UTM Zone 16 --> Units in m
st_transform(ward98, 3435) # Illinois State Plane --> Units in ft

# Some Plotting
ggplot(ward92, aes(fill = COUNT)) + 
        geom_sf()

ggplot(ward98, aes(fill = COUNT)) + 
        geom_sf()


# Finding the Centroids for Wards
centroids92 = st_centroid(ward92)
ggplot() + 
        geom_sf(data = ward92) +
        geom_sf(data = centroids92)


