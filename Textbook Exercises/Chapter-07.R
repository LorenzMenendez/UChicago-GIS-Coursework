# Chapter 7 — Geographic Data I/O
library(sf)
library(raster)
library(dplyr)
library(spData)

# 7.2 Retrieving Open Data ------------------------------------------------

# Download data from static URLs like so
download.file(url = "http://nrdata.nps.gov/programs/lands/nps_boundary.zip",
              destfile = "nps_boundary.zip")
unzip(zipfile = "nps_boundary.zip")
usa_parks = st_read(dsn = "nps_boundary.shp")


# 7.3 Geographic Data Packages --------------------------------------------

# Country boundaries using "rnaturalearth" package
library(rnaturalearth)
usa = ne_countries(country = "United States of America") # United States borders
class(usa)

# Climate data using "rnaturalearth" package
library(raster)
worldclim_prec = getData(name = "worldclim", var = "prec", res = 10)
class(worldclim_prec)

# OSM data using "osmdata" package 
library(osmdata)
parks = opq(bbox = "leeds uk") %>% 
        add_osm_feature(key = "leisure", value = "park") %>% 
        osmdata_sf()


# 7.4 Geographic Web Services ---------------------------------------------

# Access APIs using the "httr" package
base_url = "http://www.fao.org/figis/geoserver/wfs"
q = list(request = "GetCapabilities")
res = httr::GET(url = base_url, query = q)
res$url # Returns a URL that can be opened in browser to see what's available

# Reading from APIs
qf = list(request = "GetFeature", typeName = "area:FAO_AREAS")
file = tempfile(fileext = ".gml")
httr::GET(url = base_url, query = qf, httr::write_disk(file))
fao_areas = sf::read_sf(file)

# Access higher-level APIs using the "ows4R" package 
library(ows4R)
wfs = WFSClient$new("http://www.fao.org/figis/geoserver/wfs",
                    serviceVersion = "1.0.0", logger = "INFO")
fao_areas = wfs$getFeatures("area:FAO_AREAS")


# 7.6 Data Input --------------------------------------------------------

# Importing from many file formats into Vector data using GDAL 
vector_filepath = system.file("shapes/world.gpkg", package = "spData")
world = st_read(vector_filepath)

# Importing from CSVs with lat/long columns using GDAL
cycle_hire_txt = system.file("misc/cycle_hire_xy.csv", package = "spData")
cycle_hire_xy = st_read(cycle_hire_txt, options = c("X_POSSIBLE_NAMES=X",
                                                    "Y_POSSIBLE_NAMES=Y"))

# Importing from CSVs with geom column using GDAL
world_txt = system.file("misc/world_wkt.csv", package = "spData")
world_wkt = read_sf(world_txt, options = "GEOM_POSSIBLE_NAMES=WKT")

# Importing from KML files using GDAL
u = "https://developers.google.com/kml/documentation/KML_Samples.kml"
download.file(u, "KML_Samples.kml")
st_layers("KML_Samples.kml") # Returns the available layers

kml = read_sf("KML_Samples.kml", layer = "Placemarks") # Select a layer(s) to import

# Importing raster data using "raster" package
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
single_layer = raster(raster_filepath) # Single Layers 

multilayer_filepath = system.file("raster/landsat.tif", package = "spDataLarge")
band3 = raster(multilayer_filepath, band = 3) # Multiple Layers
multilayer_brick = brick(multilayer_filepath)
multilayer_stack = stack(multilayer_filepath)


# 7.7 Data Output ---------------------------------------------------------

# Write to many output file types using "sf"
st_write(obj = world, dsn = "world.gpkg")

st_write(obj = world, dsn = "world.gpkg", layer_options = "OVERWRITE=YES") # For overwriting
st_write(obj = world, dsn = "world.gpkg", delete_layer = TRUE) # Also for overwriting

# Write to CSV lat/long or geom using "sf"
st_write(cycle_hire_xy, "cycle_hire_xy.csv", layer_options = "GEOMETRY=AS_XY")
st_write(world_wkt, "world_wkt.csv", layer_options = "GEOMETRY=AS_WKT")

# Write rasters to many output types using "raster" package
writeRaster(x = single_layer,
            filename = "my_raster.tif",
            datatype = "INT2U") #NB: There are so mant different datatypes depending on the extent of the raster values, which them out before exporting to find the best suited one

# Write plots as images using base R 
png(filename = "lifeExp.png", width = 500, height = 350) # also pdf(), bmp(), jpeg(), tiff()

# Write plots as images maps with "tmap"
library(tmap)
tmap_obj = tm_shape(world) +
        tm_polygons(col = "lifeExp")
tmap_save(tm  = tmap_obj, filename = "lifeExp_tmap.png")

# Write interactive maps in HTML using "mapview"
library(mapview)
mapview_obj = mapview(world, zcol = "lifeExp", legend = TRUE)
mapshot(mapview_obj, file = "my_interactive_map.html")




