library(dplyr)
library(sqldf)
library(tidyverse)
library(leaflet)

##################################################################################

#plot an interactive map of Trussell Trust foodbank locations

#################################################################################

#https://towardsdatascience.com/making-interactive-maps-in-r-with-less-than-15-lines-of-code-bfd81f587e12
FBpostcodesLL %>% 
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  addMarkers(label = FBpostcodesLL$Name, 
             clusterOptions = markerClusterOptions())

