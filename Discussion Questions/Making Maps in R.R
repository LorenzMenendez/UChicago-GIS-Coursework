# Landcover Map of Zion National Park -------------------------------------
library(sf)
library(dplyr)
library(tmap)

# Importing data 
zion = st_read((system.file("vector/zion.gpkg", package = "spDataLarge"))) %>%
        as('Spatial')


data(nlcd, package = "spDataLarge") 

# Clipping Raster to Zion National Park
zion_lc = crop(nlcd, zion) %>%
        mask(zion)

# Making the Map
tmap_mode("plot")

colors = as.character(levels(zion_lc)[[1]]$colors)

tm_shape(zion_lc) + 
        tm_raster("levels", palette = colors) + 
        
        tm_layout("Land Cover", 
                  title.size = 1, 
                  title.position = c("center", "top"),
                  legend.position = c("right", "top")) +
        
        tm_compass(type = "8star", position = c("left", "center")) +
                tm_scale_bar(size = 1, position = c("left", "bottom"))




# Map of Africa HDI -------------------------------------------------------
library(spData)
library(dplyr)
library(tmap)

# Creating Eastern Africa
east_africa = world %>% 
        filter(continent == "Africa", !is.na(iso_a2)) %>% 
        left_join(worldbank_df, by = "iso_a2") %>% 
        dplyr::select(name, subregion, gdpPercap, HDI, pop_growth) %>% 
        st_transform("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25") %>%
        filter(subregion == "Eastern Africa")

# Making maps
africa1 = tm_shape(east_africa) +
        tm_polygons("HDI") + 
        tm_facets(by = "name", nrow = 8)
        
africa2 = tm_shape(east_africa) + 
        tm_polygons("pop_growth") +
        tm_facets(by = "name", nrow = 8)

africaALL = tmap_arrange(africa1, africa2)

africaALL




