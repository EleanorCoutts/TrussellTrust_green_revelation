library(dplyr)
library(sqldf)
library(tidyverse)
library(leaflet)

#####
# This is a script to plot the TT food bank
# locations on an interactive map.
#
# It also inspects the foodbank postcode and region data 
# supplied  and concludes that there are errors in the 
# region data. There are duplicate records for a number of 
# foodbanks which contain conflicting region information. 
#
#####

#Read in food bank location data from TT
#---------------------------------------------------
FBpostcodes <- read_excel('./data/FB Postcodes.xlsx',.name_repair = "universal") 
FBregions <- read.csv('./data/Food Bank Locations.csv')

#Inspect region data
#--------------------

#Check rows in FBregions are unique
names(FBregions)[names(FBregions)=="ï..Fulfilled.Foodbank.Name"] <- "Name"
FBregionsTest <- sqldf("select distinct Name from FBregions")
#There are 433 rows in FBregions. 
#Only 423 of them have a distinct Name

#Investigate which are duplicates
x <- data.frame(table(FBregions$Name))
subset(x,x$Freq>1)
#Just use FBpostcodes data and follow up FBregions dataset with Dave

#Plot FBpostcodes data on a map
#-------------------------------

#Check to see if postcodes in FBpostcodes dataset are unique
names(FBpostcodes)[names(FBpostcodes)=="Post.Code"] <- "Postcode"
FBpostcodesTest <- sqldf("select distinct postcode from FBpostcodes")
#463 out of 466 unique

#Read in Uk postcode data downloaded from freemaptools.com
#https://www.freemaptools.com/download-uk-postcode-lat-lng.htm
#https://www.doogal.co.uk/UKPostcodes.php 
Df_UK <- read.csv('./data/postcodes.csv')

#create a subset of postcode lat and long data for FB postcodes
pclist <- as.list(unique(FBpostcodes$Postcode))
datamap <- subset(Df_UK, Df_UK$Postcode %in% pclist, select= c("Postcode","Latitude","Longitude"))  
#row.names(datamap) <- 1:nrow(datamap)

#Merge the postcode lat and long data with the FB postcodes data
FBpostcodesLL <- merge(FBpostcodes,datamap,"Postcode",all=TRUE)

#plot an interactive map
#https://towardsdatascience.com/making-interactive-maps-in-r-with-less-than-15-lines-of-code-bfd81f587e12
FBpostcodesLL %>% 
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  addMarkers(label = FBpostcodesLL$Name, 
             clusterOptions = markerClusterOptions())

