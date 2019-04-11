# [Lecture 3 — Attribute Data Operations](https://docs.google.com/presentation/d/1N0B8u4OBPr9n0mbZgKP-8MX1pl28L6USs8h90VyetUs/edit#slide=id.g57bb6aab40_0_104)
## Vector Attribute Operations
* sf objects: Extends the data.frame, storing spatial and non-spatial data in the same way. One column per attribute value and one row per observation. Geometry is plotted in *geom* column. 
* Inspect data dimensions: `dim(world)` --> Rows and Columns -or- `nrow(world)` and `ncol(world)`
* Base R Subsetting: `world[1:6, ]` --> Grab rows 1-6, `world[, 1:3]` --> Grab columns 1-3, `world[,c("nam e_long", "lifeExp")]` --> Grab Name and Life Exp. columns
        * On the fly, `sel_area = world$area_km2 < 10000` will return rows where `area_km2` is less than 10k. 
* `dplyr` Subsetting: ALlows you to do more major grabs and changes quickly (i.e. column name grabs and changes, columns between two columns...). `slice(world, 3:5)` is like `select()`, `filter()` is also really useful. 
* Pipe Operators `%>%`: Equivalent to "then", i.e. do function 1, then function 2, then function 3 to the data. 
* Aggregating in Base R: `aggregate(pop ~ continent, FUN = sum, data = world, na.rm = TRUE)` 
* Aggregating using sf: `aggregate(world["pop"], by = list(world$continent), FUN = sum, na.rm = TRUE)`
* Dropping the Geometry Column: `st_drop_geometry()`

### Joins
* dplyr has `left_join()`, and `inner_join()` functions. Specify `by =` argument if there are no column header matches across the two datasets.
* `setdiff(column1, column2)` will show you what didn't join in your data. 

### Creating new variables
* It's good data practice to create new datasets instead of overwriting over the original data
* Concatenating columns together using `unite("Column Name", sep = ",")`
* Separate columns using `separate("Original Column", c("New Column 1", "New Column 2", sep = ","))`
* You can use `rename()` and `set_names()` to change the name of columns
* Drop the geometry using `st_drop_geometry()`

## Raster Attribute Operations
* Creating a raster dataset from scratch using `raster(nrows =, ncols =, res =, xmn =, xmx =, ymn =, ymx = vals= )`
* Subset using the base R `[]` operators like `elev[1,1]` --> Cell 1,1 of elevation raster
* Overwrite using the same `[]` operator like `elev[1,1] = 0` --> Cell 1,1 gets value 0. 
* Descriptive Raster Stats using `summary()` or `cellStats()`
* Visualize using `hist()`, `boxplot()` ... 