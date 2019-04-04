## Chapter 2 

# 2.1 Intro, Packages and Librarying -------------------------------------------------
# Installing Recommended Packages
install.packages("sf")
install.packages("raster")
install.packages("spData")
install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
        
        # spDataLarge didn't install because "World" is not found, troubleshooting...
        install.packages("devtools")
        library(devtools)
        # nvm, just realized I never loaded the package! 

#Loading Packages
library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data

# Viewing Package Documentation
vignette(package = "sf") # see which vignettes are available
vignette("sf1")          # an introduction to the package



# 2.2 Vector Data -------------------------------------------------------------

# Listing the column names in the "word" dataset
names(world)
plot(world)                     # plots multiple maps for each variable (column) in the datatable 
summary(world["lifeExp"])       # gives statistical overview of the variable "lifeExp"

# Understanding sf objects
world_mini = world[1:2, 1:3]
world_mini

        # Transforming from sf to sp 'spatial' class 
        library(sp)
        world_sp = as(world, Class = "Spatial")
        
        # sp to sf 
        world_sf = st_as_sf(world_sp, "sf")

# 2.2.3 Basic Map Making
plot(world[3:6])
plot(world["pop"])

        # Creates a map where Asian countries are combined
        world_asia = world[world$continent == "Asia", ]
        asia = st_union(world_asia)
        plot(world["pop"], reset = FALSE)
        plot(asia, add = TRUE, col = "red")

# 2.2.4 Base Plot Arguments
        
        # Plotting circles that represent a varible
        plot(world["continent"], reset = FALSE)
        cex = sqrt(world$pop) / 10000
        world_cents = st_centroid(world, of_largest = TRUE)
        plot(st_geometry(world_cents), add = TRUE, cex = cex)
        
        # Focusing in on an area of the map 
        india = world[world$name_long == "India", ]
        plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
        plot(world_asia[0], add = TRUE)

# 2.2.5 Geometry Types
        
        ## Points
        st_point(c(5, 2))                 # XY point
        st_point(c(5, 2, 3))              # XYZ point
        st_point(c(5, 2, 1), dim = "XYM") # XYM point
        st_point(c(5, 2, 3, 1))           # XYZM point
        
        ## Multipoints
        multipoint_matrix = rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
        st_multipoint(multipoint_matrix)
        
        ## Linestrings
        linestring_matrix = rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
        st_linestring(linestring_matrix)
        
        ## Polygons
        polygon_list = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
        st_polygon(polygon_list)
        
        ## Polygon with a hole
        polygon_border = rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))
        polygon_hole = rbind(c(2, 4), c(3, 4), c(3, 3), c(2, 3), c(2, 4))
        polygon_with_hole_list = list(polygon_border, polygon_hole)
        st_polygon(polygon_with_hole_list)
        
        ## Multilingstring
        multilinestring_list = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                                    rbind(c(1, 2), c(2, 4)))
        st_multilinestring((multilinestring_list))
        
        ## Multipolygon 
        multipolygon_list = list(list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))),
                                 list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2))))
        st_multipolygon(multipolygon_list)
        
        ## Geometry Collection
        gemetrycollection_list = list(st_multipoint(multipoint_matrix),
                                      st_linestring(linestring_matrix))
        st_geometrycollection(gemetrycollection_list)

# 2.2.7 Simple Feature Columns 
        
        ## Combining simple features into one object 
        point1 = st_point(c(5, 2))
        point2 = st_point(c(1, 3))
        points_sfc = st_sfc(point1, point2)
        points_sfc
        
        ## Combining simple features with different geom types into one object 
        point_multilinestring_sfc = st_sfc(point1, multilinestring1)
        st_geometry_type(point_multilinestring_sfc)
        
        ## Setting SRID
        points_sfc_wgs = st_sfc(point1, point2, crs = 4326)
        st_crs(points_sfc_wgs)
        
# 2.2.8 The sf class 
        ## representing a temperature of 25°C in London on June 21st, 2017
        lnd_point = st_point(c(0.1, 51.5))                 # sfg object
        lnd_geom = st_sfc(lnd_point, crs = 4326)           # sfc object
        lnd_attrib = data.frame(                           # data.frame object
                name = "London",
                temperature = 25,
                date = as.Date("2017-06-21")
        )
        lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)    # sf object
        class(lnd_sf)

        
# 2.3 Raster Data ---------------------------------------------------------

#2.3.1 Intro to Raster
        ## Importing a raster file
        raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
        new_raster = raster(raster_filepath)
        
# 2.3.2 Raster Classes
        ## Creating a Raster from Scratch
        new_raster2 = raster(nrows = 6, ncols = 6, res = 0.5, 
                             xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
                             vals = 1:36)
        
        ## Importing a satellite image using RasterBrick
        multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
        r_brick = brick(multi_raster_file)
        nlayers(r_brick)  # Shows the number of layers in the brick
        

# 2.4 Coordinate Reference Systems ----------------------------------------
        
        ## Search for available CRS systems using
        crs_data = rgdal::make_EPSG()
        View(crs_data)
        
        ## Reset the CRS using 
        new_vector = st_set_crs(new_vector, 4326) # set CRS
        
        ## Get CRS from raster
        projection(new_raster) # get CRS
        
        ## Set Raster CRS, have to use proj4string format
        projection(new_raster) = "+proj=utm +zone=12 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
                            +units=m +no_defs" # set CRS
        
        ## Units are natively supported
        luxembourg = world[world$name_long == "Luxembourg", ]
        st_area(luxembourg)
        units::set_units(st_area(luxembourg), km^2) # Change units of the result
        
        

# 2.6 Exercises -----------------------------------------------------------

        ## Creating a focused map of Nigeria
        world_africa = world[world$continent == "Africa", ]
        nigeria = world[world$name_long == "Nigeria", ]
        plot(st_geometry(nigeria), expandBB = c(1, 0.5, 0.5, 0.1), col = "#008753", lwd = 1.5)
        plot(world_africa[0], add = TRUE)
        text(8, 9, "Nigeria")
        
        ## Create an empty 10x10 raster
        my_raster = raster(nrows = 10, ncols = 10, res = .5, 
                             xmn = -2.5, xmx = 2.5, ymn = -2.5, ymx = 2.5,
                             vals = NULL)
        ## Assign random number between 0 and 10 to each cell 
        values(my_raster) <- runif(ncell(my_raster), min=0, max=10)
        
        