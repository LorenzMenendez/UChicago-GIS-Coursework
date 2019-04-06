# Chapter 3 

# Required Packages -------------------------------------------------------
library(sf)
library(raster)
library(dplyr)
library(stringr) # for working with strings (pattern matching)
library(spData)





# 3.2 Vector Attribute Manipulation ---------------------------------------
methods(class = "sf") # methods for sf objects, first 12 shown

        ## Getting basic infomration about a data table
        dim(world) # it is a 2 dimensional object, with rows and columns
        nrow(world) # how many rows?
        ncol(world) # how many columns?

        ## Dropping the Geometry Column (useful when geom takes up too much RAM)
        world_df = st_drop_geometry(world)
        class(world_df)

# 3.2.1 Vector Attribute Subsetting
world[1:6, ] # subset rows by position
world[, 1:3] # subset columns by position
world[, c("name_long", "lifeExp")] # subset columns by name

        ## Filter nations whose surface area is smaller than 10,000 km2
        sel_area = world$area_km2 < 10000
        summary(sel_area) # a logical vector
        small_countries = world[sel_area, ]
        
        small_countries = subset(world, area_km2 < 10000) # Simplest command
        
world1 = dplyr::select(world, name_long, pop) #note sticky geom column
names(world1)

        ## Select all columns between name_long and pop (inclusive)
        world2 = dplyr::select(world, name_long:pop)
        
        ## Select all columns except subregion and area_km2 (inclusive)
        world3 = dplyr::select(world, -subregion, -area_km2)
        
        ## You can select and rename at the same time
        world4 = dplyr::select(world, name_long, population = pop)
        names(world4)
        
        ## To select a single vector feature, you need to pull 
        world$pop
        pull(world, pop)
        
        ## Selecting but for rows using slice().
        slice(world, 3:5)
        world6 = filter(world, lifeExp > 82) # Filtering
        
        ## Piping in R "%>%"
        world7 = world %>%
                filter(continent == "Asia") %>%
                dplyr::select(name_long, continent) %>%
                slice(1:5)

# 3.2.2 Vector Attribute Aggregation
world_agg1 = aggregate(pop ~ continent, FUN = sum, data = world, na.rm = TRUE) # Aggregate by continent
class(world_agg1)
world_agg2 = aggregate(world["pop"], by = list(world$continent), # Aggregate by continent using sf:aggregate()
                       FUN = sum, na.rm = TRUE)
class(world_agg2)        
        
        ## Using dplyr to find the 3 most populous continents 
        world %>% 
                dplyr::select(pop, continent) %>% 
                group_by(continent) %>% 
                summarize(pop = sum(pop, na.rm = TRUE), n_countries = n()) %>% 
                top_n(n = 3, wt = pop) %>%
                st_drop_geometry() 

# 3.2.3 Vector Attribute Joining
        ## Joining world dataset sf object with non-spatial data =
        world_coffee = left_join(world, coffee_data)
        class(world_coffee) # Returns an sf object 
        plot(world_coffee["coffee_production_2017"])
        
        ## Inner Joins to prevent NAs 
        world_coffee_inner = inner_join(world, coffee_data)
        setdiff(coffee_data$name_long, world$name_long) # Finds which records didn't match
        
        ## Joining non=spatial data with world dataset sf object
        coffee_world = left_join(coffee_data, world)
        class(coffee_world) # Returns a tidyverse tibble

# 3.2.4 Creating attributes and removing spatial information
        ## Creating new columns using base R
        world_new = world # do not overwrite our original data
        world_new$pop_dens = world_new$pop / world_new$area_km2
        
        ## Using dplyr
        world %>% 
                mutate(pop_dens = pop / area_km2) # Adds a penultimate column
        world %>% 
                transmute(pop_dens = pop / area_km2) # Only retains calculated column and geom
        
        ## Merging Columns using unite(), Unmerging using separate()
        world_unite = world %>%
                unite("con_reg", continent:region_un, sep = ":", remove = TRUE)
        world_separate = world_unite %>% 
                separate(con_reg, c("continent", "region_un"), sep = ":")
        
        ## Renaming Columns 
        world %>% 
                rename(name = name_long) # one at a time
        
        new_names = c("i", "n", "c", "r", "s", "t", "a", "p", "l", "gP", "geom")
        world %>% 
                set_names(new_names) #In Bulk



# 3.3 Manipulating Raster Objects -----------------------------------------
        
        ## Creating a NUMERIC elevation raster to use for the rest of the section 
        elev = raster(nrows = 6, ncols = 6, res = 0.5,
                      xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
                      vals = 1:36)
        
        ## Creating a CATEGORICAL sand grain type raster
        grain_order = c("clay", "silt", "sand")
        grain_char = sample(grain_order, 36, replace = TRUE)
        grain_fact = factor(grain_char, levels = grain_order)
        grain = raster(nrows = 6, ncols = 6, res = 0.5, 
                       xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
                       vals = grain_fact)

# 3.3.1 Raster Subsetting
        
        ## By Row/Column
        elev[1, 1]
        
        ## By Cell ID
        elev[1]
        
        ## Extracting a layer from a stack 
        r_stack = stack(elev, grain)
        names(r_stack) = c("elev", "grain")
        raster::subset(r_stack, "elev") #Method 1
        r_stack[["elev"]] #Method 2
        r_stack$elev #Method 3
        
        ## Modifying Existing Cell Values
        elev[1, 1] = 0

# 3.3.2 Summarizing Raster Objects
        
        ## Get Raster Cell Value Statustics using
        cellStats(elev, sd)
        
        ## Hisatogram
        hist(elev)
        