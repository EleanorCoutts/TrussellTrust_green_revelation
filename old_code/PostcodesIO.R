library(PostcodesioR)
library(purrr)
library(dplyr)

#This code assumes that you have already run the ImportData code

#Get data for the first 100 postcodes in the FBpostcodes data frame using
#Postcodes.io via API using PostcodesioR package
pc_list <- as.list(FBpostcodes[1:100,"Post.Code"])
names(pc_list) <- c("postcodes")
bulk_lookup_result <- bulk_postcode_lookup(pc_list)
bulk_list <- lapply(bulk_lookup_result, "[[", 2)
bulk_df <- map_dfr(bulk_list,`[`,c("postcode", "admin_district", "country"))


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
  bulk_df <- map_dfr(bulk_list,`[`,c("postcode", "admin_district", "lsoa", "admin_ward"))
  fullbulk_df <- rbind(fullbulk_df,bulk_df)
}

#Doh, just read here - https://cran.r-project.org/web/packages/PostcodesioR/vignettes/Introduction.html
#that Scpttish postcodes need downloading using a different function