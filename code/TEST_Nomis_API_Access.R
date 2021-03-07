library(nomisr)

#########################################################

#AIM: Test out use of nomisr package to download Nomis data

#########################################################


#FIND OUT DATASET ID FOR CHOSEN DATASET

#Get a 'tibble' with metadata for all available datasets
a <- nomis_data_info()

#Try and find our dataset ID specifically for the Annual 
#population survey

b <- nomis_search(name = '*population*', 
                  keywords = 'unemployment')

#The ID for the generic Annual population survey is 
#NM_17_1


#GET CONCEPTS AND CONCEPT VALUES

#Get concepts
c <- nomis_get_metadata(id = "NM_17_1")
print(c)

#Get concept values for Geography
d <- nomis_get_metadata(id = "NM_17_1", concept = "GEOGRAPHY")
print(d)

#Get all available geography types
e <- nomis_get_metadata(id = "NM_17_1", concept = "geography", type = "type")
print(e)
#	local authorities: district / unitary is TYPE432

#Get concept values for cell
f <- nomis_get_metadata(id = "NM_17_1", concept = "CELL")
print(f)
#ID for T01:16 (All aged 16 & over - Unemployed : All People)
#is 402720257

#Get concept values for Measures
g <- nomis_get_metadata(id = "NM_17_1", concept = "MEASURES")
print(g)
#ID for value is 20100

#DOWNLOAD DATASET

#Download data:
#Dataset ID - Annual Population Survey
#Time - 01/2015 to 01/2020
#Geography - all LAD
#Cell - number of people aged 16 & over who are Unemployed
#Measures - the value
z <- nomis_get_data(id = "NM_17_1", time = "2015-01-2020-01", 
                    geography = "TYPE432", cell="402720257",
                    measures="20100", tidy=TRUE)

