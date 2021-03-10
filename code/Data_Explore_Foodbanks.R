library(tidyverse)
library(readxl)
library(stringr)

# A Quick Look at Trussell Trust foodbank data for 2019 2020 from Dave Massey

foodbanks<- read_csv('./data/Total Fed By Fiscal Year (2).csv' )
colnames(foodbanks)<- c("foodbank", "year", "vouchers")

foodbanks <- foodbanks %>%
  slice(-c(1,2)) %>%  #drop 2 rows that are a filter not data
  # change year from FY 20xx character sting to numeric 20xx 
  mutate(year = as.integer(gsub("FY ","",year)))%>%
  group_by(foodbank) %>%
  mutate(prevYrVouchers=lag(vouchers))  #add a column of difference in vouchers over previous year

# elementary statistics
foodbanks %>%
  filter (!is.na(vouchers))%>%
  group_by(year) %>%
  summarise(aveVouchers=mean(vouchers), medVouchers=median(vouchers), 
            sdVouchers=sd(vouchers))

#create intermediary data set 
foodbanksValid <- foodbanks %>%
  filter (!is.na(vouchers))

# now look at simple BoxSpline plots
ggplot(foodbanksValid, aes(group=year, x=year, y=vouchers)) + 
  geom_boxplot()

ggplot(foodbanksValid, aes(group=year, x=year, y=log(vouchers))) +
  geom_boxplot(outlier.color = "hotpink")

#Density plot for log data
ggplot(foodbanksValid, aes(group=year, x=log(vouchers), color=year)) +
  geom_density()

foodbanksValid %>% 
  filter(year ==2020 & prevYrVouchers>0) %>% 
  ggplot(aes( x=year, y=vouchers-prevYrVouchers)) +
  geom_boxplot()

foodbanksValid %>% 
  filter(year ==2020 &  prevYrVouchers>0) %>% 
  ggplot(aes(x=year, y=log(vouchers) - log(prevYrVouchers))) + 
  geom_boxplot()

## Look at a simple Linear Model
foodbanksValid %>% 
  filter(year==2020 & prevYrVouchers >= 0) %>%
  ggplot(aes(x=prevYrVouchers,y=vouchers)) + 
  geom_point() +
  stat_smooth(method="lm", col="dodgerblue3") + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to 2019 and 2020 Foodbank Data")

linearMod = foodbanksValid %>% 
  filter(year ==2020 &  prevYrVouchers>0) %>% 
  lm(vouchers ~ prevYrVouchers,.)

# summary statistics
summary(linearMod)
BIC(linearMod)

#But statistics misleading as data is not normal; residuals are heavy tailed (see Mathematica ouput)
#
#look a log data (log 10 for convenience)
# Scatter Plot of log data
foodbanksValid %>% 
  filter(year ==2020 &  prevYrVouchers>0) %>% 
  ggplot(aes( x=log(prevYrVouchers,10), y=log(vouchers,10))) +
  geom_point()

foodbanksValid %>% 
  filter(year ==2020 &  prevYrVouchers>300) %>% 
  ggplot(aes( x=log(prevYrVouchers,10), y=log(vouchers,10))) + 
  geom_point()

linearLogMod= foodbanksValid %>% 
  filter(year ==2020 &  prevYrVouchers>0) %>% 
  lm(log(vouchers,10) ~ log(prevYrVouchers,10),.)
summary(linearLogMod)
BIC(linearLogMod)

# but again residuals are not normal
#  plot log (1 - empirical distribution function ) vs log x for the tail
#
# etc
### add in Geogrpahic data #####
## eg add in district, lOcal authority and lat and long columns  to the data 


     