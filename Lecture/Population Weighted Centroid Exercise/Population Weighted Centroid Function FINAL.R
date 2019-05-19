# Population Weighted Centroid Function 


PopulationCentroid <- function(LittleAreas, BigAreas) {
        
        #0 Inits
                # final base R data frame. This will get filled as the algortihm does through iterations 
                wgtdCentroids = data.frame("X" = numeric(), "Y" = numeric(), "POP" = integer())
        
                # Variable Names
                litAs = LittleAreas
                bigAs = BigAreas 
        
        #1 Iteration over rows of Big Areas 'bigAs'
        for (row in 1:nrow(bigAs)) {
                bigA = bigAs[row,]
                
                #2. Select/Confirm litA within bigA
                litA = st_join(bigA, litAs, join = st_contains)
                
                #3. Return a data.frame of normal centroid coordiantes in 'X' 'Y' columns, with corresponding population in 'POP' column
                subACentroid = st_centroid(litA) %>%
                        st_coordinates() %>%
                        as.data.frame() %>%
                        cbind(as.data.frame(litA$POP.y)) %>%
                        rename(POP = `litA$POP.y`)
                
                #4. Calculate Weighted Mean Centroid
                # wi * xi, wi * yi
                subACentroid$xWgtd = with(subACentroid, X * POP)
                subACentroid$yWgtd = with(subACentroid, Y * POP)
                
                # X, Y, bigPOP
                bigPOP = sum(subACentroid$POP)
                bigX = sum(subACentroid$xWgtd) / bigPOP
                bigY = sum(subACentroid$yWgtd) / bigPOP
                
                
                # Creating a data.frame with X,Y and POP columns
                bigXY = data.frame("X" = bigX, "Y" = bigY, "POP" = bigPOP)
                
                # Appending 'bigXY' to initialized data.frame 'wgtdCentroids'
                wgtdCentroids = rbind(wgtdCentroids, bigXY)
                
                
        }
        
        # 5. Converting final data.frame to 'sf'object
        wgtdCentroids_sf = na.omit(wgtdCentroids) %>% # Will have to figure out how to handle NAs
                st_as_sf(coords = c("X", "Y"), crs = st_crs(bigAs))
        
        WeightedCentroids_sf <<- wgtdCentroids_sf
}