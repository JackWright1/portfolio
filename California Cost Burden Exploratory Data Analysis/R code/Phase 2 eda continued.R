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
unique(cleanhousingdata$race_eth_name)
unique(cleanhousingdata$region_name)
unique(cleanhousingdata$burden)
unique(cleanhousingdata$tenure)
unique(cleanhousingdata$county_name)

### state total percent x race
whiteca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(race_eth_name=="White") 
mean(whiteca$percent)

raceca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(!race_eth_name=="Total") 
mean(raceca$percent) 
summary(raceca$percent)

aaca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aaca$percent)

asianca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(race_eth_name=="Asian") 
mean(asianca$percent)

latinoca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(race_eth_name=="Latino") 
mean(latinoca$percent)

nhca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhca$percent)

aica <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(race_eth_name=="AIAN") 
mean(aica$percent)

mca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(race_eth_name=="Multiple") 
mean(mca$percent)

##################################### time for bay area
whiteba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(race_eth_name=="White") 
mean(whiteba$percent)

raceba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(!race_eth_name=="Total") 
mean(raceba$percent) ## 
summary(raceba$percent)

aaba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aaba$percent)

asianba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(race_eth_name=="Asian") 
mean(asianba$percent)

latinoba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(race_eth_name=="Latino") 
mean(latinoba$percent)

nhba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhba$percent)

aiba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(race_eth_name=="AIAN") 
mean(aiba$percent)

mba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(race_eth_name=="Multiple") 
mean(mba$percent)

##################################### time for butte
whiteb <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(race_eth_name=="White") 
mean(whiteb$percent)

raceb <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(!race_eth_name=="Total") 
mean(raceb$percent) ## 
summary(raceb$percent)

aab <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aab$percent)

asianb <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(race_eth_name=="Asian") 
mean(asianb$percent)

latinob <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(race_eth_name=="Latino") 
mean(latinob$percent)

nhb <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhb$percent)

aib <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(race_eth_name=="AIAN") 
mean(aib$percent)

mb <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(race_eth_name=="Multiple") 
mean(mb$percent)

#################################### time for central/southeast sierra
whitecss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(race_eth_name=="White") 
mean(whitecss$percent)

racecss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(!race_eth_name=="Total") 
mean(racecss$percent) ## 
summary(racecss$percent)

aacss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aacss$percent)

asiancss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(race_eth_name=="Asian") 
mean(asiancss$percent)

latinocss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(race_eth_name=="Latino") 
mean(latinocss$percent)

nhcss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhcss$percent)

aicss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(race_eth_name=="AIAN") 
mean(aicss$percent)

mcss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(race_eth_name=="Multiple") 
mean(mcss$percent)

#################################################### time for monterey bay
whitemb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(race_eth_name=="White") 
mean(whitemb$percent)

racemb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(!race_eth_name=="Total") 
mean(racemb$percent) ## 
summary(racemb$percent)

aamb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aamb$percent)

asianmb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(race_eth_name=="Asian") 
mean(asianmb$percent)

latinomb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(race_eth_name=="Latino") 
mean(latinomb$percent)

nhmb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhmb$percent)

aimb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(race_eth_name=="AIAN") 
mean(aimb$percent)

mmb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(race_eth_name=="Multiple") 
mean(mmb$percent)

######################################## time for North Coast
whitenc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(race_eth_name=="White") 
mean(whitenc$percent)

racenc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(!race_eth_name=="Total") 
mean(racenc$percent) ## 
summary(racenc$percent)

aanc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aanc$percent)

asiannc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(race_eth_name=="Asian") 
mean(asiannc$percent)

latinonc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(race_eth_name=="Latino") 
mean(latinonc$percent)

nhnc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhnc$percent)

ainc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(race_eth_name=="AIAN") 
mean(ainc$percent)

mnc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(race_eth_name=="Multiple") 
mean(mnc$percent)

######################################## Northeast Sierra
whitens <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(race_eth_name=="White") 
mean(whitens$percent)

racens <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(!race_eth_name=="Total") 
mean(racens$percent) ## 
summary(racens$percent)

aans <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aans$percent)

asianns <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(race_eth_name=="Asian") 
mean(asianns$percent)

latinons <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(race_eth_name=="Latino") 
mean(latinons$percent)

nhns <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhns$percent)

ains <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(race_eth_name=="AIAN") 
mean(ains$percent)

mns <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(race_eth_name=="Multiple") 
mean(mns$percent)

########################################## Northern Sacramento Valley
whitensv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(race_eth_name=="White") 
mean(whitensv$percent)

racensv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(!race_eth_name=="Total") 
mean(racensv$percent) ## 
summary(racensv$percent)

aansv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aansv$percent)

asiannsv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(race_eth_name=="Asian") 
mean(asiannsv$percent)

latinonsv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(race_eth_name=="Latino") 
mean(latinonsv$percent)

nhnsv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhnsv$percent)

ainsv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(race_eth_name=="AIAN") 
mean(ainsv$percent)

mnsv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(race_eth_name=="Multiple") 
mean(mnsv$percent)

############################################### Sacramento Area
whitesa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(race_eth_name=="White") 
mean(whitesa$percent)

racesa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(!race_eth_name=="Total") 
mean(racesa$percent) ## 
summary(racesa$percent)

aasa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aasa$percent)

asiansa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(race_eth_name=="Asian") 
mean(asiansa$percent)

latinosa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(race_eth_name=="Latino") 
mean(latinosa$percent)

nhsa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhsa$percent)

aisa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(race_eth_name=="AIAN") 
mean(aisa$percent)

msa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(race_eth_name=="Multiple") 
mean(msa$percent)

######################################### San Diego
whitesd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(race_eth_name=="White") 
mean(whitesd$percent)

racesd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(!race_eth_name=="Total") 
mean(racesd$percent) ## 
summary(racesd$percent)

aasd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aasd$percent)

asiansd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(race_eth_name=="Asian") 
mean(asiansd$percent)

latinosd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(race_eth_name=="Latino") 
mean(latinosd$percent)

nhsd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhsd$percent)

aisd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(race_eth_name=="AIAN") 
mean(aisd$percent)

msd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(race_eth_name=="Multiple") 
mean(msd$percent)

###################################### San Joaquin Valley
whitesjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(race_eth_name=="White") 
mean(whitesjv$percent)

racesjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(!race_eth_name=="Total") 
mean(raceb$percent) ## 
summary(racesjv$percent)

aasjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aasjv$percent)

asiansjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(race_eth_name=="Asian") 
mean(asiansjv$percent)

latinosjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(race_eth_name=="Latino") 
mean(latinosjv$percent)

nhsjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhsjv$percent)

aisjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(race_eth_name=="AIAN") 
mean(aib$percent)

msjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(race_eth_name=="Multiple") 
mean(msjv$percent)

############################################ San Luis Obispo
whiteslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(race_eth_name=="White") 
mean(whiteslo$percent)

raceslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(!race_eth_name=="Total") 
mean(raceslo$percent) ## 
summary(raceslo$percent)

aaslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aaslo$percent)

asianslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(race_eth_name=="Asian") 
mean(asianslo$percent)

latinoslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(race_eth_name=="Latino") 
mean(latinoslo$percent)

nhslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhslo$percent)

aislo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(race_eth_name=="AIAN") 
mean(aislo$percent)

mslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(race_eth_name=="Multiple") 
mean(mslo$percent)

############################################## Santa Barbara
whitesb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(race_eth_name=="White") 
mean(whitesb$percent)

racesb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(!race_eth_name=="Total") 
mean(raceb$percent) ## 
summary(racesb$percent)

aasb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aasb$percent)

asiansb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(race_eth_name=="Asian") 
mean(asiansb$percent)

latinosb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(race_eth_name=="Latino") 
mean(latinosb$percent)

nhsb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhsb$percent)

aisb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(race_eth_name=="AIAN") 
mean(aisb$percent)

msb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(race_eth_name=="Multiple") 
mean(msb$percent)

########################################### Shasta
whites <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(race_eth_name=="White") 
mean(whites$percent)

races <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(!race_eth_name=="Total") 
mean(races$percent) ## 
summary(races$percent)

aas <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aas$percent)

asians <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(race_eth_name=="Asian") 
mean(asians$percent)

latinos <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(race_eth_name=="Latino") 
mean(latinos$percent)

nhs <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhs$percent)

ais <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(race_eth_name=="AIAN") 
mean(ais$percent)

ms <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(race_eth_name=="Multiple") 
mean(ms$percent)

################################### Southern California
whitesc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(race_eth_name=="White") 
mean(whitesc$percent)

racesc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(!race_eth_name=="Total") 
mean(racesc$percent) ## 
summary(racesc$percent)

aasc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(race_eth_name=="AfricanAm") 
mean(aasc$percent)

asiansc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(race_eth_name=="Asian") 
mean(asiansc$percent)

latinosc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(race_eth_name=="Latino") 
mean(latinosc$percent)

nhsc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(race_eth_name=="NHOPI") 
mean(nhsc$percent)

aisc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(race_eth_name=="AIAN") 
mean(aisc$percent)

msc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(race_eth_name=="Multiple") 
mean(msc$percent)

######################################### percent x tenure
##################################state total

tenureca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenureca

ggplot(tenureca, aes(y=percent, x=tenure)) + geom_boxplot()

### bay area
tenureba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenureba

ggplot(tenureba, aes(y=percent, x=tenure)) + geom_boxplot()
### butte
tenurebutte <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenurebutte

ggplot(tenurebutte, aes(y=percent, x=tenure)) + geom_boxplot()
### central soiutheast sierra
tenurecss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenurecss

ggplot(tenurecss, aes(y=percent, x=tenure)) + geom_boxplot()
### monterey bay
tenuremb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenuremb

ggplot(tenuremb, aes(y=percent, x=tenure)) + geom_boxplot()
### north coast
tenurenc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenurenc

ggplot(tenurenc, aes(y=percent, x=tenure)) + geom_boxplot()
### northeast sierra
tenurens <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenurens

ggplot(tenurens, aes(y=percent, x=tenure)) + geom_boxplot()
### northern sac valley
tenurensv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenurensv

ggplot(tenurensv, aes(y=percent, x=tenure)) + geom_boxplot()
### sac area
tenuresa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenuresa

ggplot(tenuresa, aes(y=percent, x=tenure)) + geom_boxplot()
### san diego
tenuresd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenuresd

ggplot(tenuresd, aes(y=percent, x=tenure)) + geom_boxplot()
### san joaquin valley
tenuresjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenuresjv

ggplot(tenuresjv, aes(y=percent, x=tenure)) + geom_boxplot()
### san luis obispo
tenureslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenureslo

ggplot(tenureslo, aes(y=percent, x=tenure)) + geom_boxplot()
### santa barb
tenuresb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenuresb

ggplot(tenuresb, aes(y=percent, x=tenure)) + geom_boxplot()
### shasta
tenureshasta <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenureshasta

ggplot(tenureshasta, aes(y=percent, x=tenure)) + geom_boxplot()
### socal
tenuresocal <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(!tenure=="Total households (owner- and renter-occupied)") %>% 
  filter(!tenure=="Combined rent-/mortgage-paying households") %>% 
  filter(!race_eth_name=="Total")
tenuresocal

ggplot(tenuresocal, aes(y=percent, x=tenure)) + geom_boxplot()
#### basic tenure done

ggplot(tenureca, aes(x=tenure, y=percent, shape=geoname, color=race_eth_name)) + geom_boxplot()
######### must repeat for all regions######### must repeat for all regions


### 4 main ones Bay area

ggplot(tenureba, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### butte

ggplot(tenurebutte, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### central southeast sierra

ggplot(tenurecss, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### mont bay

ggplot(tenuremb, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### north coast

ggplot(tenurenc, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### northeast sierra

ggplot(tenurens, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### n sac valley

ggplot(tenurensv, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### sac area

ggplot(tenuresa, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### san diego

ggplot(tenuresd, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### san joaquin valley

ggplot(tenuresjv, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### san luis obispo

ggplot(tenureslo, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### santa barb

ggplot(tenuresb, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### shasta

ggplot(tenureshasta, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
### socal

ggplot(tenuresocal, aes(x=tenure, y=percent, shape=region_name, color=race_eth_name)) + geom_boxplot()
#### end of those 4

#### race tenure, # households
#### state total Owner and Renter
whiteca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Renter-occupied households")
sum(whiteca$total_households)

aaca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Renter-occupied households")
sum(aaca$total_households)

asianca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Renter-occupied households")
sum(asianca$total_households)

latinoca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Renter-occupied households")
sum(latinoca$total_households)

nhca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Renter-occupied households")
sum(nhca$total_households)

aica <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Renter-occupied households")
sum(aica$total_households)

mca <- cleanhousingdata %>% 
  filter(geoname=="California") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Renter-occupied households")
sum(mca$total_households)

##################################### time for bay area
whiteba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Owner-occupied households")
sum(whiteba$total_households)

aaba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Owner-occupied households")
sum(aaba$total_households)

asianba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Owner-occupied households")
sum(asianba$total_households)

latinoba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Owner-occupied households")
sum(latinoba$total_households)

nhba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Owner-occupied households")
sum(nhba$total_households)

aiba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Owner-occupied households")
sum(aiba$total_households)

mba <- cleanhousingdata %>% 
  filter(region_name=="Bay Area") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Owner-occupied households")
sum(mba$total_households)

##################################### time for butte 
whiteb <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Owner-occupied households")
sum(whiteb$total_households)

aab <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Owner-occupied households")
sum(aab$total_households)

asianb <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Owner-occupied households")
sum(asianb$total_households)

latinob <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Owner-occupied households")
sum(latinob$total_households)

nhb <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Owner-occupied households")
sum(nhb$total_households)

aib <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Owner-occupied households")
sum(aib$total_households)

mb <- cleanhousingdata %>% 
  filter(region_name=="Butte") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Owner-occupied households")
sum(mb$total_households)

#################################### time for central/southeast sierra Renter
whitecss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Renter-occupied households")
sum(whitecss$total_households)

aacss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Renter-occupied households")
sum(aacss$total_households)

asiancss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Renter-occupied households")
sum(asiancss$total_households)

latinocss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Renter-occupied households")
sum(latinocss$total_households)

nhcss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Renter-occupied households")
sum(nhcss$total_households)

aicss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Renter-occupied households")
sum(aicss$total_households)

mcss <- cleanhousingdata %>% 
  filter(region_name=="Central/Southeast Sierra") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Renter-occupied households")
sum(mcss$total_households)

#################################################### time for monterey bay Owner
whitemb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Owner-occupied households")
sum(whitemb$total_households)

aamb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Owner-occupied households")
sum(aamb$total_households)

asianmb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Owner-occupied households")
sum(asianmb$total_households)

latinomb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Owner-occupied households")
sum(latinomb$total_households)

nhmb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Owner-occupied households")
sum(nhmb$total_households)

aimb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Owner-occupied households")
sum(aimb$total_households)

mmb <- cleanhousingdata %>% 
  filter(region_name=="Monterey Bay") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Owner-occupied households")
sum(mmb$total_households)

######################################## time for North Coast Owner
whitenc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Owner-occupied households")
sum(whitenc$total_households)

aanc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Owner-occupied households")
sum(aanc$total_households)

asiannc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Owner-occupied households")
sum(asiannc$total_households)

latinonc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Owner-occupied households")
sum(latinonc$total_households)

nhnc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Owner-occupied households")
sum(nhnc$total_households)

ainc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Owner-occupied households")
sum(ainc$total_households)

mnc <- cleanhousingdata %>% 
  filter(region_name=="North Coast") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Owner-occupied households")
sum(mnc$total_households)

######################################## Northeast Sierra Renter
whitens <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Renter-occupied households")
sum(whitens$total_households)

aans <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Renter-occupied households")
sum(aans$total_households)

asianns <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Renter-occupied households")
sum(asianns$total_households)

latinons <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Renter-occupied households")
sum(latinons$total_households)

nhns <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Renter-occupied households")
sum(nhns$total_households)

ains <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Renter-occupied households")
sum(ains$total_households)

mns <- cleanhousingdata %>% 
  filter(region_name=="Northeast Sierra") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Renter-occupied households")
sum(mns$total_households)

########################################## Northern Sacramento Valley Renter
whitensv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Renter-occupied households")
sum(whitensv$total_households)

aansv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Renter-occupied households")
sum(aansv$total_households)

asiannsv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Renter-occupied households")
sum(asiannsv$total_households)

latinonsv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Renter-occupied households")
sum(latinonsv$total_households)

nhnsv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Renter-occupied households")
sum(nhnsv$total_households)

ainsv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Renter-occupied households")
sum(ainsv$total_households)

mnsv <- cleanhousingdata %>% 
  filter(region_name=="Northern Sacramento Valley") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Renter-occupied households")
sum(mnsv$total_households)

############################################### Sacramento Area Renter
whitesa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Renter-occupied households")
sum(whitesa$total_households)

aasa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Renter-occupied households")
sum(aasa$total_households)

asiansa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Renter-occupied households")
sum(asiansa$total_households)

latinosa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Renter-occupied households")
sum(latinosa$total_households)

nhsa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Renter-occupied households")
sum(nhsa$total_households)

aisa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Renter-occupied households")
sum(aisa$total_households)

msa <- cleanhousingdata %>% 
  filter(region_name=="Sacramento Area") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Renter-occupied households")
sum(msa$total_households)

######################################### San Diego 
whitesd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Renter-occupied households")
sum(whitesd$total_households)

aasd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Renter-occupied households")
sum(aasd$total_households)

asiansd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Renter-occupied households")
sum(asiansd$total_households)

latinosd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Renter-occupied households")
sum(latinosd$total_households)

nhsd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Renter-occupied households")
sum(nhsd$total_households)

aisd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Renter-occupied households")
sum(aisd$total_households)

msd <- cleanhousingdata %>% 
  filter(region_name=="San Diego") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Renter-occupied households")
sum(msd$total_households)

###################################### San Joaquin Valley
whitesjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Renter-occupied households")
sum(whitesjv$total_households)

aasjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Renter-occupied households")
sum(aasjv$total_households)

asiansjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Renter-occupied households")
sum(asiansjv$total_households)

latinosjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Renter-occupied households")
sum(latinosjv$total_households)

nhsjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Renter-occupied households")
sum(nhsjv$total_households)

aisjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Renter-occupied households")
sum(aisjv$total_households)

msjv <- cleanhousingdata %>% 
  filter(region_name=="San Joaquin Valley") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Renter-occupied households")
sum(msjv$total_households)

############################################ San Luis Obispo 
whiteslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Renter-occupied households")
sum(whiteslo$total_households)

aaslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Renter-occupied households")
sum(aaslo$total_households)

asianslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Renter-occupied households")
sum(asianslo$total_households)

latinoslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Renter-occupied households")
sum(latinoslo$total_households)

nhslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Renter-occupied households")
sum(nhslo$total_households)

aislo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Renter-occupied households")
sum(aislo$total_households)

mslo <- cleanhousingdata %>% 
  filter(region_name=="San Luis Obispo") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Renter-occupied households")
sum(mslo$total_households)

############################################## Santa Barbara 
whitesb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Renter-occupied households")
sum(whitesb$total_households)

aasb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Renter-occupied households")
sum(aasb$total_households)

asiansb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Renter-occupied households")
sum(asiansb$total_households)

latinosb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Renter-occupied households")
sum(latinosb$total_households)

nhsb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Renter-occupied households")
sum(nhsb$total_households)

aisb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Renter-occupied households")
sum(aisb$total_households)

msb <- cleanhousingdata %>% 
  filter(region_name=="Santa Barbara") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Renter-occupied households")
sum(msb$total_households)

########################################### Shasta
whites <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Renter-occupied households")
sum(whites$total_households)

aas <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Renter-occupied households")
sum(aas$total_households)

asians <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Renter-occupied households")
sum(asians$total_households)

latinos <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Renter-occupied households")
sum(latinos$total_households)

nhs <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Renter-occupied households")
sum(nhs$total_households)

ais <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Renter-occupied households")
sum(ais$total_households)

ms <- cleanhousingdata %>% 
  filter(region_name=="Shasta") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Renter-occupied households")
sum(ms$total_households)

################################### Southern California
whitesc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(race_eth_name=="White") %>% 
  filter(tenure=="Renter-occupied households")
sum(whitesc$total_households)

aasc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(race_eth_name=="AfricanAm") %>% 
  filter(tenure=="Renter-occupied households")
sum(aasc$total_households)

asiansc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(race_eth_name=="Asian") %>% 
  filter(tenure=="Renter-occupied households")
sum(asiansc$total_households)

latinosc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(race_eth_name=="Latino") %>% 
  filter(tenure=="Renter-occupied households")
sum(latinosc$total_households)

nhsc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(race_eth_name=="NHOPI") %>% 
  filter(tenure=="Renter-occupied households")
sum(nhsc$total_households)

aisc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(race_eth_name=="AIAN") %>% 
  filter(tenure=="Renter-occupied households")
sum(aisc$total_households)

msc <- cleanhousingdata %>% 
  filter(region_name=="Southern California") %>% 
  filter(race_eth_name=="Multiple") %>% 
  filter(tenure=="Renter-occupied households")
sum(msc$total_households)
