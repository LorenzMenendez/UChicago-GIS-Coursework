# Chapter 4 
library(sf)
library(raster)
library(dplyr)
library(spData)

# 4.2 Spatial Operations on Vector Data -----------------------------------

# 4.2.1 Spatial Subsetting
canterbury = nz %>% filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]

        ## Using the "[" operator
        nz_height[canterbury, , op = st_disjoint] # Returns all points not in Canterbury

# 4.2.2 Topological Relations

# create a polygon
a_poly = st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1))))
a = st_sfc(a_poly)
# create a line
l_line = st_linestring(x = matrix(c(-1, -1, -0.5, 1), ncol = 2))
l = st_sfc(l_line)
# create points
p_matrix = matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2)
p_multi = st_multipoint(x = p_matrix)
p = st_cast(st_sfc(p_multi), "POINT")
        
        ## Do the objects intersect?
        st_intersects(p, a) # Output will be 1 or (empty)
        st_intersects(p, a, sparse = FALSE) # Makes output TRUE or FALSE 
        
        ## Do the objects not intersect?
        st_disjoint(p, a, sparse = FALSE)[, 1] # Returns a vector
        st_within(p, a, sparse = FALSE)[, 1] # Objects have to be fully within
        st_touches(p, a, sparse = FALSE)[, 1] # Objects cross border of polygon
        
        ## Are the objects within a certain distance of each other?
        sel = st_is_within_distance(p, a, dist = 0.9) # can only return a sparse matrix
        lengths(sel) > 0
        
# 4.2.3 Spatial Joining
        
        ## Creating random points on earth's surface for the exercise
        set.seed(2018) # set seed for reproducibility
        (bb_world = st_bbox(world)) # the world's bounds
        random_df = tibble(
                x = runif(n = 10, min = bb_world[1], max = bb_world[3]),
                y = runif(n = 10, min = bb_world[2], max = bb_world[4])
        )
        random_points = random_df %>% 
                st_as_sf(coords = c("x", "y")) %>% # set coordinates
                st_set_crs(4326) # set geographic CRS
        
        ## Subset countries that have points, then join name 
        world_random = world[random_points, ]
        nrow(world_random)
        random_joined = st_join(random_points, world["name_long"]) # Join name
        
# 4.2.4 Non-Overlapping Joins
        
        ## Plotting Cycle data in London from official sources
        plot(st_geometry(cycle_hire), col = "blue")
        plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")
        
        ## Check to see if any points are the same
        any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE))
        
        ## Adding non-overlapping cycle hire data from OSM
        cycle_hire_P = st_transform(cycle_hire, 27700)
        cycle_hire_osm_P = st_transform(cycle_hire_osm, 27700)
        sel = st_is_within_distance(cycle_hire_P, cycle_hire_osm_P, dist = 20)
        summary(lengths(sel) > 0)
        
        ## Joining official with OSM data
        z = st_join(cycle_hire_P, cycle_hire_osm_P, st_is_within_distance, dist = 20)
        nrow(cycle_hire) # But this returns multiple matches! So Aggregate
        
        ## Aggregating the multiple matches 
        z = z %>% 
                group_by(id) %>% 
                summarize(capacity = mean(capacity))
        nrow(z) == nrow(cycle_hire)
        
        ## Plotting the results 
        plot(cycle_hire_osm["capacity"])
        plot(z["capacity"])

# 4.2.5 Spatial Data Aggregation
        
        ## Finding Average Height in areas in New Zealand 
        nz_avheight = aggregate(x = nz_height, by = nz, FUN = mean)
        
        ## Aggregating from incongruent areas using area weaighted aggregation
        agg_aw = st_interpolate_aw(incongruent[, "value"], aggregating_zones,
                                   extensive = TRUE)
        agg_aw$value

# 4.2.6 Distance Relations
        
        ## Calculating distances
        nz_heighest = nz_height %>% top_n(n = 1, wt = elevation)
        canterbury_centroid = st_centroid(canterbury)
        st_distance(nz_heighest, canterbury_centroid) # Gives units and returns as matrix
        
        co = filter(nz, grepl("Canter|Otag", Name)) # Getting distance between multiple points
        st_distance(nz_height[1:3, ], co)



# 4.3 Spatial Operations on Raster Data -----------------------------------

# 4.3.1 Spatial Subsetting
        
        ## Find the value of the cell that covers a point located 0.1 units from the origin
        id = cellFromXY(elev, xy = c(0.1, 0.1))
        elev[id]
        #or 
        raster::extract(elev, data.frame(x = 0.1, y = 0.1))
        
        ## Subsetting a chunk of raster based another raster or vector object
        clip = raster(xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45,
                      res = 0.3, vals = rep(1, 9))
        elev[clip]
        
        ## Subsetting a chunk of raster, but returning a spatial object
        elev[1:2, drop = FALSE]    # spatial subsetting with cell IDs
        elev[1, 1:2, drop = FALSE] # spatial subsetting by row,column indices

        ## Creating Masks an subsetting
        rmask = elev 
        values(rmask) = sample(c(NA, TRUE), 36, replace = TRUE)
        
        elev[rmask, drop = FALSE]           # with [ operator
        mask(elev, rmask)                   # with mask()
        overlay(elev, rmask, fun = "max")   # with overlay

# 4.3.3 Local Operations
        
        ## Reclassifying an elevation raster into low, med, high elevation
        rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
        recl = reclassify(elev, rcl = rcl)

# 4.3.4 Focal Operations (i.e. Analyzing cells around a central cell)
        
        ## Finding the minimum value in a moving 3x3 window
        r_focal = focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min)

# 4.3.5 Zonal Operations (i.e. a focal operations with a non-rectangular window)
        
        ## Grain-sizes
        z = zonal(elev, grain, fun = "mean") %>%
                as.data.frame()
        z

# 4.3.7 Merging Rasters 
        
        ## Getting the elevation raster for CH and A 
        aut = getData("alt", country = "AUT", mask = TRUE)
        ch = getData("alt", country = "CHE", mask = TRUE)
        aut_ch = merge(aut, ch)
        
        