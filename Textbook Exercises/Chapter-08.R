# Chapter 8
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)

# Visualization Packages
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse vis package
library(shiny)   # for web applications

# 8.2 Static Maps -------------------------------------------------------

# 8.2.1 tmap Basics
        # Add fill layer to nz shape
        tm_shape(nz) +
                tm_fill() 
        # Add border layer to nz shape
        tm_shape(nz) +
                tm_borders() 
        # Add fill and border layers to nz shape
        tm_shape(nz) +
                tm_fill() +
                tm_borders() 

# 8.2.2 Map Objects
        map_nz = tm_shape(nz) + tm_polygons()
        class(map_nz) # Will return "tmap" class
        
        # Map with multiple data layers
        map_nz1 = map_nz +
                tm_shape(nz_elev) + tm_raster(alpha = 0.7)
        
        # Adding layers to an existing map
        nz_water = st_union(nz) %>% st_buffer(22200) %>% 
                st_cast(to = "LINESTRING")
        
        map_nz2 = map_nz1 +
                tm_shape(nz_water) + tm_lines()
        
        map_nz3 = map_nz2 +
                tm_shape(nz_height) + tm_dots()
        
        # Plot multiple maps in the same view
        tmap_arrange(map_nz1, map_nz2, map_nz3)

# 8.2.3 Aesthetic Functions
        ma1 = tm_shape(nz) + tm_fill(col = "red")
        ma2 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3)
        ma3 = tm_shape(nz) + tm_borders(col = "blue")
        ma4 = tm_shape(nz) + tm_borders(lwd = 3)
        ma5 = tm_shape(nz) + tm_borders(lty = 2)
        ma6 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3) +
                tm_borders(col = "blue", lwd = 3, lty = 2)
        tmap_arrange(ma1, ma2, ma3, ma4, ma5, ma6)
        
        #NB: vector$column arguments won't work with tmap, you need
        tm_shape(nz) + tm_fill(col = "Land_area")
        
        # Creating a legend
        legend_title = expression("Area (km"^2*")")
        map_nza = tm_shape(nz) +
                tm_fill(col = "Land_area", title = legend_title) + tm_borders()

# 8.2.4 Color Settings
        
        # Simple Choropleth maps
        tm_shape(nz) + tm_polygons(col = "Median_income")
        breaks = c(0, 3, 4, 5) * 10000
        tm_shape(nz) + tm_polygons(col = "Median_income", breaks = breaks) # also can use pretty, equal, quantile, jenks, cont and cat 
        tm_shape(nz) + tm_polygons(col = "Median_income", n = 10)
        tm_shape(nz) + tm_polygons(col = "Median_income", palette = "BuGn")

# 8.2.5 Layouts
        
        # Creating compass and scale bars
        map_nz + 
                tm_compass(type = "8star", position = c("left", "top")) +
                tm_scale_bar(breaks = c(0, 100, 200), size = 1)
        
        # Using high-level map themes
        map_nza + tm_style("bw")
        map_nza + tm_style("classic")
        map_nza + tm_style("cobalt")
        map_nza + tm_style("col_blind")

# 8.2.6 Faceted Maps (i.e. maps for different timescales)
        urb_1970_2030 = urban_agglomerations %>% 
                filter(year %in% c(1970, 1990, 2010, 2030))
        tm_shape(world) + tm_polygons() + 
                tm_shape(urb_1970_2030) + tm_symbols(col = "black", border.col = "white",
                                                     size = "population_millions") +
                tm_facets(by = "year", nrow = 2, free.coords = FALSE)

# 8.2.7 Inset Maps
        
        # First you have to create the map you want to inset
        nz_region = st_bbox(c(xmin = 1340000, xmax = 1450000,
                              ymin = 5130000, ymax = 5210000),
                            crs = st_crs(nz_height)) %>% 
                st_as_sfc()
        
        # Then create the basemap
        nz_height_map = tm_shape(nz_elev, bbox = nz_region) +
                tm_raster(style = "cont", palette = "YlGn", legend.show = TRUE) +
                tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 1) +
                tm_scale_bar(position = c("left", "bottom"))
        
        # Then create the inset map
        nz_map = tm_shape(nz) + tm_polygons() +
                tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 0.1) + 
                tm_shape(nz_region) + tm_borders(lwd = 3) 
        
        # Combine in a plot
        library(grid)
        nz_height_map
        print(nz_map, vp = viewport(0.8, 0.27, width = 0.5, height = 0.5))

        



# 8.3 Animated Maps -------------------------------------------------------

# Creates and animated map of agglomerations between 1950 and 2030
urb_anim = tm_shape(world) + tm_polygons() + 
        tm_shape(urban_agglomerations) + tm_dots(size = "population_millions") +
        tm_facets(along = "year", free.coords = FALSE)

tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 25)


        


# 8.4 Interactive Maps ----------------------------------------------------

# Toggling between static and interactive
tmap_mode("view")
map_nz

# "Mapview" package is useful because you can quickly append it to the end of pipes
trails %>%
        st_transform(st_crs(franconia)) %>%
        st_intersection(franconia[franconia$district == "Oberfranken", ]) %>%
        st_collection_extract("LINE") %>%
        mapview(color = "red", lwd = 3, layer.name = "trails") +
        mapview(franconia, zcol = "district", burst = TRUE) +
        breweries

# "Mapdeck" allows you to create 2.5D maps with extrusions for data points

library(mapdeck)
set_token(Sys.getenv("MAPBOX")) # NB: You need to create a Mapbox access token first https://www.mapbox.com/help/how-access-tokens-work/
crash_data = read.csv("https://git.io/geocompr-mapdeck")
crash_data = na.omit(crash_data)
ms = mapdeck_style("dark")
mapdeck(style = ms, pitch = 45, location = c(0, 52), zoom = 4) %>%
        add_grid(data = crash_data, lat = "lat", lon = "lng", cell_size = 1000,
                 elevation_scale = 50, layer_id = "grid_layer",
                 colour_range = viridisLite::plasma(6))

# "Leaflet" gives the most stability and fuctionnality 
pal = colorNumeric("RdYlBu", domain = cycle_hire$nbikes)
leaflet(data = cycle_hire) %>% 
        addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
        addCircles(col = ~pal(nbikes), opacity = 0.9) %>% 
        addPolygons(data = lnd, fill = FALSE) %>% 
        addLegend(pal = pal, values = ~nbikes) %>% 
        setView(lng = -0.1, 51.5, zoom = 12) %>% 
        addMiniMap()




# 8.5 Mapping Applications ------------------------------------------------


# Creates a quick application where you can slide life expectancy
library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(spData)   # loads the world dataset 
ui = fluidPage(
        sliderInput(inputId = "life", "Life expectancy", 49, 84, value = 80),
        leafletOutput(outputId = "map")
)
server = function(input, output) {
        output$map = renderLeaflet({
                leaflet() %>% addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
                        addPolygons(data = world[world$lifeExp < input$life, ])})
}
shinyApp(ui, server)


# 8.6 Other Mapping Packages ----------------------------------------------

# Using "ggplot2"
library(ggplot2)
g1 = ggplot() + geom_sf(data = nz, aes(fill = Median_income)) +
        geom_sf(data = nz_height) +
        scale_x_continuous(breaks = c(170, 175))
g1

# "Cartogram" will create geometry distorted and non-contiugous area maps
nz_carto = cartogram_cont(nz, "Median_income", itermax = 5)
tm_shape(nz_carto) + tm_polygons("Median_income")

us_states2163 = st_transform(us_states, 2163)
us_states2163_ncont = cartogram_ncont(us_states2163, "total_pop_15")
us_states2163_dorling = cartogram_dorling(us_states2163, "total_pop_15")
