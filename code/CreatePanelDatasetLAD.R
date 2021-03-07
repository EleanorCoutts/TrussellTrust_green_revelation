library(stringr) 
library(sqldf)

###################################################################################

#Create panel dataset by LAD

###################################################################################

###Link Trussell Trust foodbanks to LADs

#create a subset of the postcode lat and long data for Trussell Trust foodbank postcodes
names(FBpostcodes)[names(FBpostcodes)=="Post.Code"] <- "Postcode"
pclist <- as.list(unique(FBpostcodes$Postcode))
datamap <- subset(Df_UK, Df_UK$Postcode %in% pclist, select= c("Postcode","Latitude","Longitude","District"))  

#Merge the postcode lat and long data with the FB postcodes data
FBpostcodesLL <- merge(FBpostcodes,datamap,"Postcode",all=TRUE)

#Merge FBPostcodesLL with TrussellTrust foodbank voucher data
FBpostcodesLL$foodbank <- str_remove(FBpostcodesLL$Name, " Foodbank")
foodBankUsage <- merge(foodbanks, FBpostcodesLL)

#Create panel dataset by LAD
LADPanel <- sqldf("select District, year, sum(vouchers) from foodBankUsage group by District, year")
colnames(LADPanel)[colnames(LADPanel)=="District"] <- "LAD"

###Add explanatory variables

unemploymentSub <- subset(unemployment, unemployment$date=="2019-12" & unemployment$item_name=="Unemployment rate (model based)")[c("geography_name","obs_value")]
unemploymentSub$year <- "2019"
colnames(unemploymentSub)[colnames(unemploymentSub)=="geography_name"] <- "LAD"
colnames(unemploymentSub)[colnames(unemploymentSub)=="obs_value"] <- "EstUnemploymentRate"

workLimitingDisabledSub <-subset(workLimitingDisabled, workLimitingDisabled$date=="2019-12")[c("geography_name","obs_value")]
workLimitingDisabledSub$year <- "2019"
colnames(workLimitingDisabledSub)[colnames(workLimitingDisabledSub)=="geography_name"] <- "LAD"
colnames(workLimitingDisabledSub)[colnames(workLimitingDisabledSub)=="obs_value"] <- "workLimitingDisabled"

explanatoryVar <- merge(unemploymentSub,workLimitingDisabledSub,by=c("LAD","year"))

LADPanelAll <- merge(LADPanel,explanatoryVar,by=c("LAD","year"),all=TRUE)

