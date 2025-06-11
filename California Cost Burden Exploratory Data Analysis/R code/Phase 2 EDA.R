library(tidyverse)
tidyverse_logo()

library(readr)

housingdata <- read.csv("Housing_cost_burden.csv")
glimpse(housingdata)

percent1 <- housingdata %>% 
  filter(!is.na(percent)) 
percent1
nrow(percent1)
summary(percent1)

cleanhousingdata <- filter(percent1, percent<100)
cleanhousingdata
#### percent has been cleaned sufficiently, now time for subsetting and eda

filter(cleanhousingdata, geoname=="California")

####### univariate

### percent 
summary(cleanhousingdata$percent)
sd(cleanhousingdata$percent)
IQR(cleanhousingdata$percent)

plot(cleanhousingdata$percent)
hist(cleanhousingdata$percent)






#########################bivariate
######## location and regional analysis
### geoname CA totals

statetotal <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  select(percent,geoname)
statetotal

summary(statetotal$percent)
sd(statetotal$percent)
IQR(statetotal$percent)

plot(statetotal$percent)
hist(statetotal$percent)

qplot(x=percent, data=statetotal, binwidth=0.5)

### region name bay area

bayarea <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  select(region_name,percent)
bayarea

summary(bayarea$percent)
sd(bayarea$percent)
IQR(bayarea$percent)

plot(bayarea$percent)
hist(bayarea$percent)

qplot(x=percent, data=bayarea, binwidth=0.5)

### region name by butte
unique(cleanhousingdata$region_name)

butte <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  select(region_name,percent)
butte

summary(butte$percent)
sd(butte$percent)
IQR(butte$percent)

plot(butte$percent)
hist(butte$percent)
qplot(x=percent, data=butte, binwidth=0.5)

### region name by centrak/southwest sierra

centralsoutheastsierra <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  select(region_name,percent)
centralsoutheastsierra

summary(centralsoutheastsierra$percent)
sd(centralsoutheastsierra$percent)
IQR(centralsoutheastsierra$percent)

plot(centralsoutheastsierra$percent)
hist(centralsoutheastsierra$percent)
qplot(x=percent, data=centralsoutheastsierra, binwidth=0.5)

### region name by monterey bay

montereybay <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  select(region_name,percent)
montereybay

summary(montereybay$percent)
sd(montereybay$percent)
IQR(montereybay$percent)

plot(montereybay$percent)
hist(montereybay$percent)
qplot(x=percent, data=montereybay, binwidth=0.5)

## region name by North coast

northcoast <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  select(region_name,percent)
northcoast

summary(northcoast$percent)
sd(northcoast$percent)
IQR(northcoast$percent)

plot(northcoast$percent)
hist(northcoast$percent)
qplot(x=percent, data=northcoast, binwidth=0.5)

## region name by northeast sierra

northeastsierra <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  select(region_name,percent)
northeastsierra

summary(northeastsierra$percent)
sd(northeastsierra$percent)
IQR(northeastsierra$percent)

plot(northeastsierra$percent)
hist(northeastsierra$percent)
qplot(x=percent, data=northeastsierra, binwidth=0.5)

## region name by northern sacremento valley

nsacvalley <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  select(region_name,percent)
nsacvalley

summary(nsacvalley$percent)
sd(nsacvalley$percent)
IQR(nsacvalley$percent)

plot(nsacvalley$percent)
hist(nsacvalley$percent)
qplot(x=percent, data=nsacvalley, binwidth=0.5)

## region name by sac area

sacarea <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  select(region_name,percent)
sacarea

summary(sacarea$percent)
sd(sacarea$percent)
IQR(sacarea$percent)

plot(sacarea$percent)
hist(sacarea$percent)
qplot(x=percent, data=sacarea, binwidth=0.5)

## region name by san diego

sandiego <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  select(region_name,percent)
sandiego

summary(sandiego$percent)
sd(sandiego$percent)
IQR(sandiego$percent)

plot(sandiego$percent)
hist(sandiego$percent)
qplot(x=percent, data=sandiego, binwidth=0.5)

## region name by san joaquin valley

sjvalley <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  select(region_name,percent)
sjvalley

summary(sjvalley$percent)
sd(sjvalley$percent)
IQR(sandiego$percent)

plot(sjvalley$percent)
hist(sjvalley$percent)
qplot(x=percent, data=sjvalley, binwidth=0.5)

## region name by slo

slo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  select(region_name,percent)
slo

summary(slo$percent)
sd(slo$percent)
IQR(slo$percent)

plot(slo$percent)
hist(slo$percent)
qplot(x=percent, data=slo, binwidth=0.5)

## region name by santa barbara

santab <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  select(region_name,percent)
santab

summary(santab$percent)
sd(santab$percent)
IQR(santab$percent)

plot(santab$percent)
hist(santab$percent)
qplot(x=percent, data=santab, binwidth=0.5)

## region name by shasta

shasta <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  select(region_name,percent)
shasta

summary(shasta$percent)
sd(shasta$percent)
IQR(shasta$percent)

plot(shasta$percent)
hist(shasta$percent)
qplot(x=percent, data=shasta, binwidth=0.5)

## region name by socal

socal <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  select(region_name,percent)
socal

summary(socal$percent)
sd(socal$percent)
IQR(socal$percent)

plot(socal$percent)
hist(socal$percent)
qplot(x=percent, data=socal, binwidth=0.5)
######## done with region name
##### time for county name ??
unique(cleanhousingdata$county_name)

## county name alameda

alameda <- cleanhousingdata %>% 
  filter(county_name=="Alameda") %>% 
  select(county_name,percent)
alameda

summary(alameda$percent)
sd(alameda$percent)
IQR(alameda$percent)

plot(alameda$percent)
hist(alameda$percent)
qplot(x=percent, data=alameda, binwidth=0.5)

## county name alpine

alpine <- cleanhousingdata %>% 
  filter(county_name=="Alpine") %>% 
  select(county_name,percent)
alpine

summary(alpine$percent)
sd(alpine$percent)
IQR(alpine$percent)

plot(alpine$percent)
hist(alpine$percent)
qplot(x=percent, data=alpine, binwidth=0.5)

## county name amador

amador <- cleanhousingdata %>% 
  filter(county_name=="Amador") %>% 
  select(county_name,percent)
amador

summary(amador$percent)
sd(amador$percent)
IQR(amador$percent)

plot(amador$percent)
hist(amador$percent)
qplot(x=percent, data=amador, binwidth=0.5)

## county name butte 

butte1 <- cleanhousingdata %>% 
  filter(county_name=="Butte") %>% 
  select(county_name,percent)
butte1

summary(butte1$percent)
sd(butte1$percent)
IQR(butte1$percent)

plot(butte1$percent)
hist(butte1$percent)
qplot(x=percent, data=butte1, binwidth=0.5)

## county name calaveras

calaveras <- cleanhousingdata %>% 
  filter(county_name=="Calaveras") %>% 
  select(county_name,percent)
calaveras

summary(calaveras$percent)
sd(calaveras$percent)
IQR(calaveras$percent)

plot(calaveras$percent)
hist(calaveras$percent)
qplot(x=percent, data=calaveras, binwidth=0.5)

## county name colusa

colusa <- cleanhousingdata %>% 
  filter(county_name=="Colusa") %>% 
  select(county_name,percent)
colusa

summary(colusa$percent)
sd(colusa$percent)
IQR(colusa$percent)

plot(colusa$percent)
hist(colusa$percent)

## county name contra costa

contrac <- cleanhousingdata %>% 
  filter(county_name=="Contra Costa") %>% 
  select(county_name,percent)
contrac

summary(contrac$percent)
sd(contrac$percent)
IQR(contrac$percent)

plot(contrac$percent)
hist(contrac$percent)

## county name del norte

eld <- cleanhousingdata %>% 
  filter(county_name=="El Dorado") %>% 
  select(county_name,percent)
eld

summary(eld$percent)
sd(eld$percent)
IQR(eld$percent)

plot(eld$percent)
hist(eld$percent)

## county name fresno

eld <- cleanhousingdata %>% 
  filter(county_name=="El Dorado") %>% 
  select(county_name,percent)
eld

summary(eld$percent)
sd(eld$percent)
IQR(eld$percent)

plot(eld$percent)
hist(eld$percent)

## county name glenn

glenn <- cleanhousingdata %>% 
  filter(county_name=="Glenn") %>% 
  select(county_name,percent)
glenn

summary(glenn$percent)
sd(glenn$percent)
IQR(glenn$percent)

plot(glenn$percent)
hist(glenn$percent)

## county name humboldt

humboldt <- cleanhousingdata %>% 
  filter(county_name=="Humboldt") %>% 
  select(county_name,percent)
humboldt

summary(humboldt$percent)
sd(humboldt$percent)
IQR(humboldt$percent)

plot(humboldt$percent)
hist(humboldt$percent)

## county name imperial

imp <- cleanhousingdata %>% 
  filter(county_name=="Imperial") %>% 
  select(county_name,percent)
imp

summary(imp$percent)
sd(imp$percent)
IQR(imp$percent)

plot(imp$percent)
hist(imp$percent)

## county name inyo

inyo <- cleanhousingdata %>% 
  filter(county_name=="Inyo") %>% 
  select(county_name,percent)
inyo

summary(inyo$percent)
sd(inyo$percent)
IQR(inyo$percent)

plot(inyo$percent)
hist(inyo$percent)

## county name kern

kern <- cleanhousingdata %>% 
  filter(county_name=="Kern") %>% 
  select(county_name,percent)
kern

summary(kern$percent)
sd(kern$percent)
IQR(kern$percent)

plot(kern$percent)
hist(kern$percent)

## county name kings

kings <- cleanhousingdata %>% 
  filter(county_name=="Kings") %>% 
  select(county_name,percent)
kings

summary(kings$percent)
sd(kings$percent)
IQR(kern$percent)

plot(kings$percent)
hist(kings$percent)

## county name lake

lake <- cleanhousingdata %>% 
  filter(county_name=="Lake") %>% 
  select(county_name,percent)
lake

summary(lake$percent)
sd(lake$percent)
IQR(lake$percent)

plot(lake$percent)
hist(lake$percent)

## county name lassen

lass <- cleanhousingdata %>% 
  filter(county_name=="Lassen") %>% 
  select(county_name,percent)
lass

summary(lass$percent)
sd(lass$percent)
IQR(lass$percent)

plot(lass$percent)
hist(lass$percent)

## county name los angeles

la <- cleanhousingdata %>% 
  filter(county_name=="Los Angeles") %>% 
  select(county_name,percent)
la

summary(la$percent)
sd(la$percent)
IQR(la$percent)

plot(la$percent)
hist(la$percent)

## county name madera

madera <- cleanhousingdata %>% 
  filter(county_name=="Madera") %>% 
  select(county_name,percent)
madera

summary(madera$percent)
sd(madera$percent)
IQR(madera$percent)

plot(madera$percent)
hist(madera$percent)

## county name marin

marin <- cleanhousingdata %>% 
  filter(county_name=="Marin") %>% 
  select(county_name,percent)
marin

summary(marin$percent)
sd(marin$percent)
IQR(marin$percent)

plot(marin$percent)
hist(marin$percent)

## county name mariposa

mp <- cleanhousingdata %>% 
  filter(county_name=="Mariposa") %>% 
  select(county_name,percent)
mp

summary(mp$percent)
sd(mp$percent)
IQR(mp$percent)

plot(mp$percent)
hist(mp$percent)

## county name medocino

mp <- cleanhousingdata %>% 
  filter(county_name=="Mariposa") %>% 
  select(county_name,percent)
mp

summary(mp$percent)
sd(mp$percent)
IQR(mp$percent)

plot(mp$percent)
hist(mp$percent)

## county name merced



## county name modoc



###### eda by race
unique(cleanhousingdata$race_eth_name)
## race total
racetotal <- cleanhousingdata %>% 
  filter(race_eth_name=="Total") %>% 
  select(race_eth_name,percent)
racetotal

summary(racetotal$percent)
sd(racetotal$percent)
IQR(racetotal$percent)

plot(racetotal$percent)
hist(racetotal$percent)

## race Asian

asian <- cleanhousingdata %>% 
  filter(race_eth_name=="Asian") %>% 
  select(race_eth_name,percent)
asian

summary(asian$percent)
sd(asian$percent)
IQR(asian$percent)

plot(asian$percent)
hist(asian$percent)

## race Latino

latino <- cleanhousingdata %>% 
  filter(race_eth_name=="Latino") %>% 
  select(race_eth_name,percent)
latino

summary(latino$percent)
sd(latino$percent)
IQR(latino$percent)

plot(latino$percent)
hist(latino$percent)

## race white

white <- cleanhousingdata %>% 
  filter(race_eth_name=="White") %>% 
  select(race_eth_name,percent)
white

summary(white$percent)
sd(white$percent)
IQR(white$percent)

plot(white$percent)
hist(white$percent)

## race african american

aa <- cleanhousingdata %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  select(race_eth_name,percent)
aa

summary(aa$percent)
sd(aa$percent)
IQR(aa$percent)

plot(aa$percent)
hist(aa$percent)

## race american indian alaska native

aian <- cleanhousingdata %>% 
  filter(race_eth_name=="AIAN") %>% 
  select(race_eth_name,percent)
aian

summary(aian$percent)
sd(aian$percent)
IQR(aian$percent)

plot(aian$percent)
hist(aian$percent)

## race native hawaiian pacific islander

nhpi <- cleanhousingdata %>% 
  filter(race_eth_name=="NHOPI") %>% 
  select(race_eth_name,percent)
nhpi

summary(nhpi$percent)
sd(nhpi$percent)
IQR(nhpi$percent)

plot(nhpi$percent)
hist(nhpi$percent)

## race multiple

multiple <- cleanhousingdata %>% 
  filter(race_eth_name=="Multiple") %>% 
  select(race_eth_name,percent)
multiple

summary(multiple$percent)
sd(multiple$percent)
IQR(multiple$percent)

plot(multiple$percent)
hist(multiple$percent)
######## end of race by %

################## multiple variables
######### race, state/region, and percent

raceca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(!race_eth_name=="Total") 
raceca

ggplot(raceca, aes(y=percent, x=race_eth_name)) + geom_boxplot()

## do it for regions (bay area) #### must redo for region name instead of geoname
unique(cleanhousingdata$region_name)

raceba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(!race_eth_name=="Total") %>% 
  select(race_eth_name, percent)
raceba

ggplot(raceba, aes(y=percent, x=race_eth_name)) + geom_boxplot()
### butte
racebutte <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(!race_eth_name=="Total")
racebutte

ggplot(racebutte, aes(y=percent, x=race_eth_name)) + geom_boxplot()
### central southeast sierra
racecss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(!race_eth_name=="Total")
racecss

ggplot(racecss, aes(y=percent, x=race_eth_name)) + geom_boxplot()
### monterey bay
racemb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(!race_eth_name=="Total")
racemb

ggplot(racemb, aes(y=percent, x=race_eth_name)) + geom_boxplot()
### north coast
racenc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(!race_eth_name=="Total")
racenc

ggplot(racenc, aes(y=percent, x=race_eth_name)) + geom_boxplot()
### northeast sierra
racens <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(!race_eth_name=="Total")
racens

ggplot(racens, aes(y=percent, x=race_eth_name)) + geom_boxplot()
### north sac valley
racensv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(!race_eth_name=="Total")
racensv

ggplot(racensv, aes(y=percent, x=race_eth_name)) + geom_boxplot()
### sac area
racesa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(!race_eth_name=="Total")
racesa

ggplot(racesa, aes(y=percent, x=race_eth_name)) + geom_boxplot()
### san diego
racesd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(!race_eth_name=="Total")
racesd

ggplot(racesd, aes(y=percent, x=race_eth_name)) + geom_boxplot()
### san joaquin 
racesj <- cleanhousingdata %>% 
  filter(geoname=="San Joaquin") %>% 
  filter(!race_eth_name=="Total")
racesj

ggplot(racesj, aes(y=percent, x=race_eth_name)) + geom_boxplot()
### san luis obispo
raceslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(!race_eth_name=="Total")
raceslo

ggplot(raceslo, aes(y=percent, x=race_eth_name)) + geom_boxplot()
### santa barb
racesb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(!race_eth_name=="Total")
racesb

ggplot(racesb, aes(y=percent, x=race_eth_name)) + geom_boxplot()
### shasta
raceshasta <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(!race_eth_name=="Total")
raceshasta

ggplot(raceshasta, aes(y=percent, x=race_eth_name)) + geom_boxplot()
### socal
racesocal <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(!race_eth_name=="Total")
racesocal

ggplot(racesocal, aes(y=percent, x=race_eth_name)) + geom_boxplot()

################### regions with race and percent done
######### go deeper into socal/ other counties? 
socalcounty <- cleanhousingdata %>% 
  filter(region_name=="Southern California")
ggplot(socalcounty, aes(x=county_name, y=percent)) + geom_boxplot()
## can use this code for other countys

####### 
unique(cleanhousingdata$burden) #### definitely need to explore 50% vs 30% burden,
## unsure if I should do it mostly here or in PowerBI
unique(cleanhousingdata$tenure) ## dont care abt tenure as much

unique(cleanhousingdata$income_level)
unique(cleanhousingdata$region_name)




#### eda on specific housing burden
hbca <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(!race_eth_name=="Total") 
hbca

ggplot(hbca, aes(y=percent, x=burden)) + geom_boxplot()

sum(is.na(cleanhousingdata$burden))


### tenure experiment
### california!
tenureca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenureca

ggplot(tenureca, aes(y=percent, x=tenure)) + geom_boxplot()

### bay area
tenureba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenureba

ggplot(tenureba, aes(y=percent, x=tenure)) + geom_boxplot()
### butte
tenurebutte <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenurebutte

ggplot(tenurebutte, aes(y=percent, x=tenure)) + geom_boxplot()
### central soiutheast sierra
tenurecss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenurecss

ggplot(tenurecss, aes(y=percent, x=tenure)) + geom_boxplot()
### monterey bay
tenuremb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenuremb

ggplot(tenuremb, aes(y=percent, x=tenure)) + geom_boxplot()
### north coast
tenurenc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenurenc

ggplot(tenurenc, aes(y=percent, x=tenure)) + geom_boxplot()
### northeast sierra
tenurens <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenurens

ggplot(tenurens, aes(y=percent, x=tenure)) + geom_boxplot()
### northern sac valley
tenurensv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenurensv

ggplot(tenurensv, aes(y=percent, x=tenure)) + geom_boxplot()
### sac area
tenuresa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenuresa

ggplot(tenuresa, aes(y=percent, x=tenure)) + geom_boxplot()
### san diego
tenuresd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenuresd

ggplot(tenuresd, aes(y=percent, x=tenure)) + geom_boxplot()
### san joaquin valley
tenuresjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenuresjv

ggplot(tenuresjv, aes(y=percent, x=tenure)) + geom_boxplot()
### san luis obispo
tenureslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenureslo

ggplot(tenureslo, aes(y=percent, x=tenure)) + geom_boxplot()
### santa barb
tenuresb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenuresb

ggplot(tenuresb, aes(y=percent, x=tenure)) + geom_boxplot()
### shasta
tenureshasta <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenureshasta

ggplot(tenureshasta, aes(y=percent, x=tenure)) + geom_boxplot()
### socal
tenuresocal <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households")
tenuresocal

ggplot(tenuresocal, aes(y=percent, x=tenure)) + geom_boxplot()
#### basic tenure done
##### socal, san joaquin val, and bay area must by considered for county analysis
tenureca1 <- tenureca %>% 
  filter(!race_eth_name=="Total")
ggplot(tenureca1, aes(x=geoname, y=percent, shape=tenure, color=race_eth_name)) + geom_point()
######### must repeat for all regions

### 4 main ones Bay area
tenureba1 <- tenureba %>% 
  filter(!race_eth_name=="Total")
ggplot(tenureba1, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### butte
tenurebutte1 <- tenurebutte %>% 
  filter(!race_eth_name=="Total")
ggplot(tenurebutte1, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### central southeast sierra
tenurecss1 <- tenurecss %>% 
  filter(!race_eth_name=="Total")
ggplot(tenurecss1, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### mont bay
tenuremb1 <- tenuremb %>%  
  filter(!race_eth_name=="Total")
ggplot(tenuremb1, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### north coast
tenurenc1 <- tenurenc %>%  
  filter(!race_eth_name=="Total")
ggplot(tenurenc1, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### northeast sierra
tenurens1 <- tenurens %>%  
  filter(!race_eth_name=="Total")
ggplot(tenurens1, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### n sac valley
tenurensv1 <- tenurensv %>%  
  filter(!race_eth_name=="Total")
ggplot(tenurensv1, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### sac area
tenuresa1 <- tenuresa %>%  
  filter(!race_eth_name=="Total")
ggplot(tenuresa1, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### san diego
tenuresd <- tenuresd %>%  
  filter(!race_eth_name=="Total")
ggplot(tenuresd, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### san joaquin valley
tenuresjv <- tenuresjv %>%  
  filter(!race_eth_name=="Total")
ggplot(tenuresjv, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### san luis obispo
tenureslo <- tenureslo %>%  
  filter(!race_eth_name=="Total")
ggplot(tenureslo, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### santa barb
tenuresb1 <- tenuresb %>%  
  filter(!race_eth_name=="Total")
ggplot(tenuresb1, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### shasta
tenuresh1 <- tenureshasta %>%  
  filter(!race_eth_name=="Total")
ggplot(tenuresh1, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### socal
tenuresocal1 <- tenuresocal %>%  
  filter(!race_eth_name=="Total")
ggplot(tenuresocal, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
#### end of those 4

############################# all 5

install.packages("SmartEDA")
library(SmartEDA)
cleanhousingdata1 <- select(cleanhousingdata, tenure, race_eth_name, region_name, geoname, percent, burden,
                            county_name, income_level, total_households, burdened_households, ind_id, ind_definition)
glimpse(cleanhousingdata1)

write.csv(cleanhousingdata1, 'cleanhousingdata1.csv') ### export selected data into csv for powerbi usage

ggplot(cleanhousingdata1, aes(x=geoname, y=percent, color=race_eth_name)) + 
  geom_point() + 
  facet_wrap(tenure ~ burden)

table(cleanhousingdata1$county_name)


