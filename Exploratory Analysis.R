
library(dplyr)
library(ggplot2)
library(stringr)


##Source file information
fileURL<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
datafile <- "StormData.csv.bz2"

## Download if it is not already in the working directory
if(!file.exists(datafile)){
  download.file(fileURL, datafile, method="curl")
}


stormData <- read.csv("StormData.csv.bz2", header = TRUE, sep = ",", quote = "\"")
stormData$EVTYPE <- toupper(stormData$EVTYPE)

##We see there are inconsistencies in event types so they need to be cleaned up

stormData$EVTYPE <- gsub('AVALANCE','AVALANCHE',stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE <- gsub('FLOODING','FLOOD',stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE <- gsub('FLOODS','FLOOD',stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE[grepl( "FLASH FLOOD", stormData$EVTYPE)] <-"FLASH FLOOD"
stormData$EVTYPE <- gsub( 'COASTALSTORM','COASTAL STORM',stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE <- gsub( 'HEAT WAVES','HEAT WAVE',stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE[grepl( "HURRICANE ERIN", stormData$EVTYPE)] <-"HURRICANE"
stormData$EVTYPE[grepl( "HURRICANE FELIX", stormData$EVTYPE)] <-"HURRICANE"
stormData$EVTYPE[grepl( "HURRICANE OPAL", stormData$EVTYPE)] <-"HURRICANE"
stormData$EVTYPE <- gsub( "LANDSLIDES","LANDSLIDE",stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE <- gsub( "LIGHTNING.","LIGHTNING",stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE <- gsub( "MUDSLIDES","MUDSLIDE",stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE <- gsub( "RIP CURRENTS","RIP CURRENT",stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE <- gsub( "SNOW SQUALLS","SNOW SQUALL",stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE <- gsub( "STRONG WINDS","STRONG WIND",stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE <- gsub( "SNOW SQUALLS","SNOW SQUALL",stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE <- gsub( "THUNDERSTORM WINDS","THUNDERSTORM WIND",stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE <- gsub( "THUNDERTORM WINDS","THUNDERSTORM WIND",stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE[grepl( "TROPICAL STORM GORDON", stormData$EVTYPE)] <-"TROPICAL STORM"
stormData$EVTYPE <- gsub( "WILDFIRE","WILD FIRE",stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE <- gsub( "WINDS","WIND",stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE <- gsub( "WINTER STORMS","WINTER STORM",stormData$EVTYPE, ignore.case = T)
stormData$EVTYPE <- gsub( "WINTRY MIX","WINTER WEATHER/MIX",stormData$EVTYPE, ignore.case = T)

## Check for NAs
list_na <- colnames(stormData)[ apply(stormData, 2, anyNA) ]
list_na

## calculate damage amounts based on exponent
NewStormData <- stormData %>% 
mutate(PROPDAMAGE = case_when(
  PROPDMGEXP == "H" || PROPDMGEXP == "h" ~ PROPDMG * 1e+02,
  PROPDMGEXP == "K" || PROPDMGEXP == "k" ~ PROPDMG * 1e+03,
  PROPDMGEXP == "M" || PROPDMGEXP == "m" ~ PROPDMG * 1e+06,
  PROPDMGEXP == "B" || PROPDMGEXP == "b" ~ PROPDMG * 1e+09,
  TRUE ~ PROPDMG),
  CROPDAMAGE = case_when(
    CROPDMGEXP == "H" || CROPDMGEXP == "h" ~ CROPDMG * 1e+02,
    CROPDMGEXP == "K" || CROPDMGEXP == "k" ~ CROPDMG * 1e+03,
    CROPDMGEXP == "M" || CROPDMGEXP == "m" ~ CROPDMG * 1e+06,
    CROPDMGEXP == "B" || CROPDMGEXP == "b" ~ CROPDMG * 1e+09,
    TRUE ~ CROPDMG)
  ) %>% 
mutate(HealthImpact = INJURIES + FATALITIES) %>%
mutate(Damage = PROPDAMAGE + CROPDAMAGE)

##HealtImpact data
HealthImpactSum <- NewStormData %>%
  group_by(EVTYPE) %>%
  summarise(TotalHealthImpact = sum(HealthImpact))

HealthImpactSum <- HealthImpactSum[with(HealthImpactSum,order(-TotalHealthImpact)),]

head(HealthImpactSum,10)

Health1 <- head(HealthImpactSum,10)
Health2 <- Health1[order(Health1$TotalHealthImpact),]
Health2$EVTYPE=factor(Health2$EVTYPE,levels=Health2$EVTYPE)
g1 <- ggplot(Health2,aes(x=factor(EVTYPE),y=TotalHealthImpact)) + 
  geom_bar(stat='identity', fill="red") + coord_flip() + 
  labs(y='Total Health Impact',x='Event Types') +
  ggtitle("Top 10 most harmful Weather events")


##Damage Impact data
DamageImpactSum <- NewStormData %>%
  group_by(EVTYPE) %>%
  summarise(TotalDamageImpact = sum(Damage))

DamageImpactSum <- DamageImpactSum[with(DamageImpactSum,order(-TotalDamageImpact)),]

head(DamageImpactSum,10)

Damage1 <- head(DamageImpactSum,10)
Damage2 <- Damage1[order(Damage1$TotalDamageImpact),]
Damage2$EVTYPE=factor(Damage2$EVTYPE,levels=Damage2$EVTYPE)
g2 <- ggplot(Damage2,aes(x=factor(EVTYPE),y=TotalDamageImpact)) + 
  geom_bar(stat='identity', fill="blue") + 
  coord_flip() + 
  labs(y='Total Damage Impact',x='Event Types') +
  ggtitle("Top 10 Weather events with greatest economic consequences")



