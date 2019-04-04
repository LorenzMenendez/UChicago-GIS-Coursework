# Chapter 1 

# A Code Snippet that makes a map of where the authors of the textbook are from
library(leaflet)
popup = c("Robin", "Jakub", "Jannes")
leaflet() %>%
        addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
        addMarkers(lng = c(-3, 23, 11),
                   lat = c(52, 53, 49), 
                   popup = popup)