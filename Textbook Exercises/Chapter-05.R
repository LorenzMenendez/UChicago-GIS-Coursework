# Chapter 5: Geometry Operations

library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(rmapshaper)


# 5.2 Geometric Operations on Vector Data ---------------------------------

# 5.2.1 Simplification
seine_simp = st_simplify(seine, dTolerance = 2000)  # Lines
us_states1 = st_transform(us_states, 2163) %>%
        st_simplify(, dTolerance = 100000) # Can mess up topography (i.e. "touching")

us_states2 = st_transform(us_states, 2163) %>%
        ms_simplify(, keep = 0.01, keep_shapes = TRUE) # Retains topography
        

        # Check object size using the object.size() function
        object.size(seine)
        object.size(seine_simp)

# 5.2.2 Centroids
nz_centroid = st_centroid(nz)
seine_centroid = st_centroid(seine)

nz_pos = st_point_on_surface(nz) # Forces centroids to be in the polygon
seine_pos = st_point_on_surface(seine)

# 5.2.3 Buffers
seine_buff_5km = st_buffer(seine, dist = 5000)
seine_buff_50km = st_buffer(seine, dist = 50000)

# 5.2.4 Affine Transformations
nz_shift = nz_sfc + c(0, 100000) # Shifts all y-coordintes 100k units to the North

nz_centroid_sfc = st_centroid(nz_sfc)
nz_scale = (nz_sfc - nz_centroid_sfc) * 0.5 + nz_centroid_sfc # Halves the size of plolygones on their centroid

rotation = function(a){ # function takes a, the rotation angle in degrees
        r = a * pi / 180 #degrees to radians
        matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2) 
} 

nz_scale_sf = st_set_geometry(nz, nz_scale) # Overwrites old geometry with the new one

# 5.2.5 Clipping (multi- spatial types ONLY)
b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
b = st_buffer(b, dist = 1) # convert points to circles
plot(b)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y")) # add text

        # Intersection Function
        x = b[1]
        y = b[2]
        x_and_y = st_intersection(x, y)
        plot(b)
        plot(x_and_y, col = "lightgrey", add = TRUE) # color intersecting area
        

# 5.2.6 Geometry Unions
regions = aggregate(x = us_states[, "total_pop_15"], by = list(us_states$REGION),
        FUN = sum, na.rm = TRUE)
regions2 = us_states %>% group_by(REGION) %>%
        summarize(pop = sum(total_pop_15, na.rm = TRUE))

# 5.2.7 Type Transformations
multipoint = st_multipoint(matrix(c(1, 3, 5, 1, 3, 1), ncol = 2))

linestring = st_cast(multipoint, "LINESTRING") # Create lines from points
polyg = st_cast(multipoint, "POLYGON") # Create polygons from lines

# Reversing what we just did
multipoint_2 = st_cast(linestring, "MULTIPOINT")
multipoint_3 = st_cast(polyg, "MULTIPOINT")
all.equal(multipoint, multipoint_2, multipoint_3)

# Creating lines from a multiline object
multilinestring_list = list(matrix(c(1, 4, 5, 3), ncol = 2), 
                            matrix(c(4, 4, 4, 1), ncol = 2),
                            matrix(c(2, 4, 2, 2), ncol = 2))
linestring_sf2 = st_cast(multilinestring_sf, "LINESTRING")
linestring_sf2

# 5.3 Geometric Operations on Raster Data ---------------------------------

# 5.3.1 Geometric Intersections

data("elev", package = "spData") # Clips a raster based on a vector object, but still returns a raster
clip = raster(xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45,
              res = 0.3, vals = rep(1, 9))
elev[clip, drop = FALSE]

# 5.3.2 Extent and Origin
data(elev, package = "spData")
elev_2 = extend(elev, c(1, 2), value = 1000)
plot(elev_2)

elev_3 = elev + elev_2 # Generates a warning message because the Rasters have different extents

elev_4 = extend(elev, elev_2) # Extends the smaller raster to the bigger one by adding NAs


origin(elev_4) = c(0.25, 0.25) # change the origin
plot(elev_4)

plot(elev, add = TRUE) # and add the original raster

# 5.3.3 Aggregation and Disaggregation

data("dem", package = "RQGIS")
dem_agg = aggregate(dem, fact = 5, fun = mean)

dem_disagg = disaggregate(dem_agg, fact = 5, method = "bilinear") # Bilinear makes a smooth interpolation
identical(dem, dem_disagg)

# 5.4 Raster-vector interactions ------------------------------------------

# 5.4.1 Raster Cropping
srtm = raster(system.file("raster/srtm.tif", package = "spDataLarge"))
zion = st_read(system.file("vector/zion.gpkg", package = "spDataLarge"))
zion = st_transform(zion, projection(srtm))

srtm_cropped = crop(srtm, zion) # Reduces the rectangular extent of the raster to only the vector object
srtm_masked = mask(srtm, zion) # Sets all raster values outside of vector to NA
srtm_inv_masked = mask(srtm, zion, inverse = TRUE) # Sets all raster values inside vector to NA

# 5.4.2 Raster extraction
data("zion_points", package = "spDataLarge") 
zion_points$elevation = raster::extract(srtm, zion_points) # Returns value on raster for given set of vector points

zion_transect = cbind(c(-113.2, -112.9), c(37.45, 37.2)) %>% # Returns valeu fo raster along a line
        st_linestring() %>% 
        st_sfc(crs = projection(srtm)) %>% 
        st_sf()

transect = raster::extract(srtm, zion_transect, 
                           along = TRUE, cellnumbers = TRUE) # Returns the elevation along a given route

zion_srtm_values = raster::extract(x = srtm, y = zion, df = TRUE)  # Returns many raster value inside of a polygon -->: Useful for calculating max/min/avg... in a polygon)

zion_nlcd = raster::extract(nlcd, zion, df = TRUE, factors = TRUE) # Counts occurences of categorical data in a raster
dplyr::select(zion_nlcd, ID, levels) %>% 
        tidyr::gather(key, value, -ID) %>%
        group_by(ID, key, value) %>%
        tally() %>% 
        tidyr::spread(value, n, fill = 0)

# NB: These raster functions can be very slow, so you can use SAGA functions from RQGIS pkg

# 5.4.3 Rasterization
cycle_hire_osm_projected = st_transform(cycle_hire_osm, 27700)
raster_template = raster(extent(cycle_hire_osm_projected), resolution = 1000,
                         crs = st_crs(cycle_hire_osm_projected)$proj4string)

# 5.4.4 Spatial Vectorization
elev_point = rasterToPoints(elev, spatial = TRUE) %>% # Returns valued centroids of raster cells
        st_as_sf()

data(dem, package = "RQGIS") # Creating countour lines
cl = rasterToContour(dem)
plot(dem, axes = FALSE)
plot(cl, add = TRUE)

# create hillshade
hs = hillShade(slope = terrain(dem, "slope"), aspect = terrain(dem, "aspect"))
plot(hs, col = gray(0:100 / 100), legend = FALSE)
# overlay with DEM
plot(dem, col = terrain.colors(25), alpha = 0.5, legend = FALSE, add = TRUE)
# add contour lines
contour(dem, col = "white", add = TRUE)

grain_poly = rasterToPolygons(grain) %>% # Converts each raster cell into a polygon 
        st_as_sf()

grain_poly2 = grain_poly %>% # Converts aggregated polygons when raster cells are same value
        group_by(layer) %>%
        summarize()

# Lecture Work

library(RQGIS)
library(dplyr)
library(sf)
data(random_points)
data(ndvi)
ch = st_combine(random_points) %>%
        st_convex_hull()

plot(ch)

nz_simplified = st_simplify(nz[1], dTolerance = 45000) %>%
        plot()


can_buffer_points = nz[1] %>% # For some reason not working
        filter(Name == 'Canterbury') %>%
        st_buffer(, dist = 100) %>%
        st_within(, nz_height) %>%
        n()
        


