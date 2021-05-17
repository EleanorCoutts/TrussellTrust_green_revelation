library(readxl)
library(nomisr)
library(PostcodesioR)
library(purrr)

############################################################################

# IMPORT DATA

############################################################################

getwd()

###TRUSSELL TRUST DATA

# Read in Trussell Trust foodbank data by quarter/FY from David Massey
foodbanks <- read.csv('./data/Total Feb each FB each quarter, calendar year.csv')
colnames(foodbanks)<- c("foodbank", "year", "quarter", "vouchers")

# Read in Trussell Trust foodbank location data from David Massey
FBpostcodes <- read_excel('./data/FB Postcodes.xlsx',.name_repair = "universal") 


###MAP DATA

#Get data for the first 100 postcodes in the FBpostcodes data frame via
#the Postcodes.io API using the PostcodesioR package
pc_list <- as.list(FBpostcodes[1:100,"Post.Code"])
names(pc_list) <- c("postcodes")
bulk_lookup_result <- bulk_postcode_lookup(pc_list)
bulk_list <- lapply(bulk_lookup_result, "[[", 2)
bulk_df <- map_dfr(bulk_list,`[`,c("postcode", "admin_district", "country",
                                   "longitude", "latitude"))

#Get data for the remaining postcodes in the FBpostcodes data frame and combine
#all postcode data into a single dataframe
fullbulk_df <- bulk_df
numPostcodes <- as.numeric(nrow(FBpostcodes))

i<-1
for (i in 1:floor(numPostcodes/100)){
  if ((i+1)*100 < numPostcodes){
    pc_list <- as.list(FBpostcodes[((i*100)+1):((i+1)*100),"Post.Code"])
  } else {
    pc_list <- as.list(FBpostcodes[((i*100)+1):numPostcodes,"Post.Code"])
  }
  names(pc_list) <- c("postcodes")
  bulk_lookup_result <- bulk_postcode_lookup(pc_list)
  bulk_list <- lapply(bulk_lookup_result, "[[", 2)
  bulk_df <- map_dfr(bulk_list,`[`,c("postcode", "admin_district", "country",
                                     "longitude", "latitude"))
  fullbulk_df <- rbind(fullbulk_df,bulk_df)
}

###NOMIS DATA

#Read in ONS model-based estimate values of unemployment from Nomis by LAD
unemployment <- nomis_get_data(id = "NM_127_1", time = "2019-01-2020-12", 
                    geography = "TYPE434", measures="20100", tidy=TRUE)

#Read in work-limited disabled from Nomis by LAD
workLimitingDisabled <- nomis_get_data(id = "NM_17_1", time = "2019-01-2020-12", 
                               geography = "TYPE434", cell = "405278465",
                               measures="20100", tidy=TRUE)

