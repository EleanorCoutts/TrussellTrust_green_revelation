library(stringr) 
library(sqldf)

###################################################################################

#Create panel dataset by LAD

###################################################################################

###Link Trussell Trust foodbanks to LADs

#Merge the postcode lat and long data with the FB postcodes data
names(FBpostcodes)[names(FBpostcodes)=="Post.Code"] <- "postcode"
FBpostcodesLL <- merge(FBpostcodes,fullbulk_df,"postcode",all=TRUE)

#Merge FBPostcodesLL with TrussellTrust foodbank voucher data
FBpostcodesLL$foodbank <- str_remove(FBpostcodesLL$Name, " Foodbank")
foodBankUsage <- merge(foodbanks, FBpostcodesLL)

#Create panel dataset by LAD
LADPanel <- sqldf("select admin_district, year, quarter, country, sum(vouchers) from foodBankUsage group by admin_district, year, quarter")
colnames(LADPanel)[colnames(LADPanel)=="admin_district"] <- "LAD"


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

