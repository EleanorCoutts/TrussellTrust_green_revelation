library(ggplot2)
library(dplyr)

##################################################################################

#Plot bubble plots

##################################################################################

##Example Bubble Plot March 10  ###

foodB2019 <- LADPanelAll %>%
filter(year==2019 ) %>%
filter(complete.cases(.))

colnames(foodB2019)[colnames(foodB2019)=="sum(vouchers)"] <- "sumVouchers"


ggplot(foodB2019, aes(x=-multDepIndex ,y=EstUnemploymentRate, size=sumVouchers,colour=Country))+
geom_point(alpha=0.5)+
  scale_size(range=c(.1,8),name="Sum of Vouchers")

