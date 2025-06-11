library(tidyverse)
tidyverse_logo()

library(readr)

housingdata <- read.csv("Housing_cost_burden.csv")
glimpse(housingdata)

##### closer look at each variable
unique(housingdata$ind_id) ## number of households

unique(housingdata$ind_definition)

unique(housingdata$datasource)

unique(housingdata$burden)

unique(housingdata$tenure)

unique(housingdata$reportyear)

unique(housingdata$race_eth_name)

unique(housingdata$income_level)

unique(housingdata$geotype)

unique(housingdata$geotypevalue)

unique(housingdata$geoname)

unique(housingdata$county_name)

unique(housingdata$county_fips)

unique(housingdata$region_name)

unique(housingdata$region_code)

unique(housingdata$CA_decile)

unique(housingdata$version)
#### now time to count for missing values

sum(is.na(housingdata$ind_definition)) # 0

sum(is.na(housingdata$datasource)) # 0

sum(is.na(housingdata$burden)) # 0

sum(is.na(housingdata$race_eth_name)) # 0

sum(is.na(housingdata$tenure)) # 0

sum(is.na(housingdata$reportyear)) # 0

sum(is.na(housingdata$income_level)) # 0

sum(is.na(housingdata$geotype)) # 0

sum(is.na(housingdata$geotypevalue)) # 0

sum(is.na(housingdata$geoname)) # 0
sum(!complete.cases(housingdata$geoname))

sum(is.na(housingdata$county_name)) # 0 (after doing complete cases check)
sum(!complete.cases(housingdata$county_name)) # better check for missing values and nas

sum(is.na(housingdata$county_fips)) # 810 missing values, must figure out why

sum(is.na(housingdata$region_name)) # should not be 0, but says 0
sum(!complete.cases(housingdata$region_name))

sum(is.na(housingdata$region_code)) # says 54 blanks, but should be way higher

sum(is.na(housingdata$total_households)) # 308082 missing values, will exclude these
sdfsd

sum(is.na(housingdata$burdened_households)) # 308082, same as total households

sum(is.na(housingdata$percent)) # 341268, may have to calculate % for some households

sum(is.na(housingdata$LL95CI)) # 343435 # may have to calculate this too

sum(is.na(housingdata$UL95CI)) # 343435, must investigate why %, CIs, and households are different

sum(is.na(housingdata$SE)) # 343435

sum(is.na(housingdata$rse)) # 362754

sum(is.na(housingdata$CA_decile)) # 488511, I dont this this variable is important

sum(is.na(housingdata$CA_RR)) # 419543, same as above

sum(is.na(housingdata$version)) # 0
### check for missing values is basically finished, but I need to beware of 
### blanks that are not counted as NAs, maybe bc they are spaces?, will use trim func to test

### outlier detection

ggplot(data=housingdata) + geom_histogram(mapping=aes(x=total_households))
ggplot(data=housingdata) + geom_boxplot(mapping=aes("households",x=total_households))

summary(housingdata$total_households) ## max is very high because this column includes
## the totals for california for each of the burden requirements

## county_name and region_name can probably be replaced by geoname
## region_code and county_fips can probably be replaced by geotypevalue

totalhouseholds <- housingdata %>% 
  filter(!is.na(total_households))
nrow(totalhouseholds)
summary(totalhouseholds)

ggplot(data=totalhouseholds) + geom_boxplot(mapping=aes("Number of Households",total_households))

summary(totalhouseholds$total_households)

## must exclude the percentages that are above 100 - these are outliers from incorrect data in the dataset

cleanhouseholds <- filter(totalhouseholds, percent<100)
cleanhouseholds

ggplot(data=correctpercent) + geom_boxplot(mapping=aes("Percent of Households",percent))

trimws(cleanhouseholds)

########## start of EDA
str(cleanhouseholds)
view(cleanhouseholds)

summary(cleanhouseholds)
## univariate analysis for variables
##categorical
## ind_definition

unique(cleanhouseholds$ind_definition)
table(cleanhouseholds$ind_definition)

prop.table(table(cleanhouseholds$ind_definition))
round(100*prop.table(table(cleanhouseholds$ind_definition)), digits=2)

ggplot(data=cleanhouseholds, aes(x=factor(ind_definition))) + geom_bar()

## datasource
table(cleanhouseholds$datasource)
prop.table(table(cleanhouseholds$datasource))
round(100*prop.table(table(cleanhouseholds$datasource)), digits=2)
ggplot(data=cleanhouseholds, aes(x=factor(datasource))) + geom_bar()

## burden
unique(cleanhouseholds$burden)
table(cleanhouseholds$burden)
prop.table(table(cleanhouseholds$burden))
round(100*prop.table(table(cleanhouseholds$burden)), digits=2)
ggplot(data=cleanhouseholds, aes(x=factor(burden))) + geom_bar()

## tenure

unique(cleanhouseholds$tenure)
table(cleanhouseholds$tenure)
prop.table(table(cleanhouseholds$tenure))
round(100*prop.table(table(cleanhouseholds$tenure)), digits=2)
ggplot(data=cleanhouseholds, aes(x=factor(tenure))) + geom_bar()

## race_eth_name

unique(cleanhouseholds$race_eth_name)
table(cleanhouseholds$race_eth_name)
prop.table(table(cleanhouseholds$race_eth_name))
round(100*prop.table(table(cleanhouseholds$race_eth_name)), digits=2)
ggplot(data=cleanhouseholds, aes(x=factor(race_eth_name))) + geom_bar()
### will need to come back to this one, should I exclude totals for these variables?

## income_level

unique(cleanhouseholds$income_level)
table(cleanhouseholds$income_level)
prop.table(table(cleanhouseholds$income_level))
round(100*prop.table(table(cleanhouseholds$income_level)), digits=2)
ggplot(data=cleanhouseholds, aes(x=factor(income_level))) + geom_bar()

## geotype

unique(cleanhouseholds$geotype)
table(cleanhouseholds$geotype)
prop.table(table(cleanhouseholds$geotype))
round(100*prop.table(table(cleanhouseholds$geotype)), digits=2)
ggplot(data=cleanhouseholds, aes(x=factor(geotype))) + geom_bar()

## region_name

unique(cleanhouseholds$region_name)
table(cleanhouseholds$region_name)
prop.table(table(cleanhouseholds$region_name))
round(100*prop.table(table(cleanhouseholds$region_name)), digits=2)
ggplot(data=cleanhouseholds, aes(x=factor(region_name))) + geom_bar()

## county_name

sum(is.na(cleanhouseholds$county_name))
unique(cleanhouseholds$county_name)
table(cleanhouseholds$county_name)
prop.table(table(cleanhouseholds$county_name))
round(100*prop.table(table(cleanhouseholds$county_name)), digits=2)
ggplot(data=cleanhouseholds, aes(x=factor(county_name))) + geom_bar()

## region_code

unique(cleanhouseholds$region_code)
table(cleanhouseholds$region_code)
prop.table(table(cleanhouseholds$region_code))
round(100*prop.table(table(cleanhouseholds$region_code)), digits=2)
ggplot(data=cleanhouseholds, aes(x=factor(region_code))) + geom_bar()

####### numerical now

## total_households
summary(cleanhouseholds$total_households)
sd(cleanhouseholds$total_households)
IQR(cleanhouseholds$total_households)

plot(cleanhouseholds$total_households)
hist(cleanhouseholds$total_households)

qplot(x=totalhouseholds, data=cleanhouseholds, binwidth=0.5) ### error message

## geoname

unique(cleanhouseholds$geoname)
table(cleanhouseholds$geoname)
prop.table(table(cleanhouseholds$geoname))
round(100*prop.table(table(cleanhouseholds$geoname)), digits=2)
ggplot(data=cleanhouseholds, aes(x=factor(geoname))) + geom_bar()

## geotypevalue

unique(cleanhouseholds$geotypevalue)
table(cleanhouseholds$geotypevalue)
prop.table(table(cleanhouseholds$geotypevalue))
round(100*prop.table(table(cleanhouseholds$geotypevalue)), digits=2)
ggplot(data=cleanhouseholds, aes(x=factor(geotypevalue))) + geom_bar()




