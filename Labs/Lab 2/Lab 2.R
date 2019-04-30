# Lab 1 

# Data and Library Loads --------------------------------------------------

# load the spatial libraries
library("sp")
library("rgdal")
library("rgeos")

# Loading Data
Census.Data <-read.csv("practicaldata.csv")

# Load the output area shapefiles
Output.Areas <- readOGR(".", "Camden_oa11")

# join our census data to the shapefile
OA.Census <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")

# load the houses point files
House.Points <- readOGR(".", "Camden_house_sales")

# A quick map of the qualification variable
library("tmap")
tm_shape(OA.Census) + tm_fill("Qualification", palette = "Reds", style = "quantile", title = "% with a Qualification") + tm_borders(alpha=.4)  

# Running the Spatial Autocorrelation (Moran's I) -------------------------------------
library(spdep)

# Calculate queen neighbors
neighbours <- poly2nb(OA.Census)
neighbours

# Plotting the neighbors to make sure everything went ok
plot(OA.Census, border = 'lightgrey')
plot(neighbours, coordinates(OA.Census), add=TRUE, col='red')

# Calculating rook neighbors
neighbours2 <- poly2nb(OA.Census, queen = FALSE)
neighbours2

# Comparing rook to queen neighbors
plot(OA.Census, border = 'lightgrey')
plot(neighbours, coordinates(OA.Census), add=TRUE, col='blue')
plot(neighbours2, coordinates(OA.Census), add=TRUE, col='red')

# Convert the neighbour data to a listw object
listw <- nb2listw(neighbours2)
listw

# global spatial autocorrelation
moran.test(OA.Census$Qualification, listw)

# creates a moran plot
moran <- moran.plot(OA.Census$Qualification, listw = nb2listw(neighbours2, style = "W"))

# creates a local moran output
local <- localmoran(x = OA.Census$Qualification, listw = nb2listw(neighbours2, style = "W"))

# binds results to our polygon shapefile
moran.map <- cbind(OA.Census, local)

# maps the results
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile", title = "local moran statistic") 




# Interpreting Moran’s I using LISA Cluster Test  -------------------------

### to create LISA cluster map ### 
quadrant <- vector(mode="numeric",length=nrow(local))

# centers the variable of interest around its mean
m.qualification <- OA.Census$Qualification - mean(OA.Census$Qualification)     

# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])    

# significance threshold
signif <- 0.1 

# builds a data quadrant
quadrant[m.qualification >0 & m.local>0] <- 4  
quadrant[m.qualification <0 & m.local<0] <- 1      
quadrant[m.qualification <0 & m.local>0] <- 2
quadrant[m.qualification >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0   

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(OA.Census,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend("bottomleft",legend=c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")



# Hot Spot Analysis wit Getis-Ord -----------------------------------------

# creates centroid and joins neighbours within 0 and 800 units
nb <- dnearneigh(coordinates(OA.Census),0,800)

# creates listw
nb_lw <- nb2listw(nb, style = 'B')

# plot the data and neighbours
plot(OA.Census, border = 'lightgrey')
plot(nb, coordinates(OA.Census), add=TRUE, col = 'red')

# compute Getis-Ord Gi statistic
local_g <- localG(OA.Census$Qualification, nb_lw)
local_g <- cbind(OA.Census, as.matrix(local_g))
names(local_g)[6] <- "gstat"

# map the results
tm_shape(local_g) + tm_fill("gstat", palette = "RdBu", style = "pretty") + tm_borders(alpha=.4)
