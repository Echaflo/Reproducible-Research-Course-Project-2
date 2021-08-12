

options(scipen = 1)  # Turn off scientific notations for numbers
library(R.utils)
library(ggplot2)
library(plyr)
require(gridExtra)
library(dplyr)




### Download the data



url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, "StormData.csv.bz2")
library(R.utils)
bunzip2("StormData.csv.bz2", "StormData.csv", overwrite=T, remove=F)
df_storm <- read.csv("StormData.csv")
names(df_storm)
str(df_storm)





#### Economic variables:

######  PROPDMG: approx. property damags
######  PROPDMGEXP: the units for property damage value
######  CROPDMG: approx. crop damages
######  CROPDMGEXP: the units for crop damage value


####   Events - target variable:
######     EVTYPE: weather event (Tornados, Wind, Snow, Flood, etc..)
######     Extract variables of interest from original data set:

####  Health variables:
* INJURIES: approx. number of injuries
* FATALITIES: approx. number of deaths
* propdamage: property damage
* cropdamage: Crop damage


head(df_storm,10)



#### Extracting variables of interest for analysis of weather impact on health and economy
#### From a list of variables in storm data, these are columns of interest:




df_storm2 <- df_storm %>% 
  select(c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")) %>%
  rename_all(tolower)
str(df_storm2)



##### Property damage estimates were entered as actual dollar amounts 


sort(table(df_storm2$propdmgexp), decreasing = TRUE)[1:10]

sort(table(df_storm2$cropdmgexp), decreasing = TRUE)[1:10]


####  There is some mess in units, so we transform those variables in one unit (dollar) variable by the following rule: 
####  * K or k: thousand dollars (10^3) 
####  * M or m: million dollars (10^6) 
####  * B or b: billion dollars (10^9) 
####  * the rest would be consider as dollars 

####  Transform EVTYPE to all caps 
####  This will improve consistency across categories 


df_storm2$evtype <- as.character(df_storm2$evtype)
df_storm2$evtype <- toupper(df_storm2$evtype)


####  Calculate property damage value 
####  Need to recode the exponent variable so we can multiply it by the propdmg value. 

  
df_storm2$propdmgexp <- toupper(df_storm2$propdmgexp)
df_storm2 <- mutate(df_storm2, propdmgexp2 = recode(propdmgexp,
                                                    "1" = 10,
                                                    "2" = 100,
                                                    "3" = 1000,
                                                    "4" = 10000,
                                                    "5" = 100000,
                                                    "6" = 1000000,
                                                    "7" = 10000000,
                                                    "8" = 100000000,
                                                    "H" = 100,
                                                    "K" = 1000,
                                                    "M" = 1000000,
                                                    "B" = 1000000000,
                                                    .default = 1)) 
df_storm2 <- mutate(df_storm2, propdmgvalue = propdmg*propdmgexp2)



###  Calculate values

####  Calculate crop damage value  
####  Need to recode the exponent variable so we can multiply it by the cropdmg value. 

  
df_storm2$cropdmgexp <- toupper(df_storm2$cropdmgexp)
df_storm2 <- mutate(df_storm2, cropdmgexp2 = recode(cropdmgexp,  
                                                    "1" = 10,
                                                    "2" = 100,
                                                    "3" = 1000,
                                                    "4" = 10000,
                                                    "5" = 100000,
                                                    "6" = 1000000,
                                                    "7" = 10000000,
                                                    "8" = 100000000,
                                                    "H" = 100,
                                                    "K" = 1000,
                                                    "M" = 1000000,
                                                    "B" = 1000000000,
                                                    .default = 1))
df_storm2 <- mutate(df_storm2, cropdmgvalue = cropdmg*cropdmgexp2)
head(df_storm2,10)


####  Subset data for analyses 
####  Injury data 

  
injuries <- aggregate(injuries~evtype, data = df_storm2, FUN = sum) 
injuries <- injuries[order(injuries$injuries, decreasing = TRUE), ] 
injuries <- injuries[1:10, ] 
injuries


####  Fatality data 
  
fatalities <- aggregate(fatalities~evtype, data = df_storm2, FUN = sum) #aggregate fatalities by event type
fatalities <- fatalities[order(fatalities$fatalities, decreasing = TRUE), ] #order number of fatalities in decending order
fatalities <- fatalities[1:10, ] 
fatalities


####  Property damage data 

propdamage <- aggregate(propdmgvalue~evtype, data = df_storm2, FUN = sum)
propdamage <- propdamage[order(propdamage$propdmgvalue, decreasing = TRUE), ]
propdamage <- propdamage[1:10, ]
propdamage


####  Crop damage data 
  
cropdamage <- aggregate(cropdmgvalue~evtype, data = df_storm2, FUN = sum)
cropdamage <- cropdamage[order(cropdamage$cropdmgvalue, decreasing = TRUE), ]
cropdamage <- cropdamage[1:10, ]
cropdamage



## The events that cause the most damage in relation to meteorological events in the United States of America are shown below. 


 

injuriesPlot <- ggplot(data=injuries, aes(x=reorder(evtype, injuries), y=injuries, color=evtype)) + 
  geom_bar(stat="identity",fill="white") + 
  xlab("Event Type") + 
  ylab("Total number injuries") +  
  ggtitle("10 Highest injuries  Events") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

injuriesPlot + coord_flip()




 

fatalitiesPlot <- ggplot(data=fatalities, aes(x=reorder(evtype, fatalities), y=fatalities, color=evtype)) + 
  geom_bar(stat="identity",fill="white") + 
  xlab("Event Type") + 
  ylab("Total number fatalities ") +  
  ggtitle("10 Highest fatalities  Events") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

fatalitiesPlot + coord_flip()



 
propdamagePlot <- ggplot(data=propdamage, aes(x=reorder(evtype, propdmgvalue), y=propdmgvalue, color=evtype)) + 
  geom_bar(stat="identity",fill="white") + 
  xlab("Event Type") + 
  ylab("Total Property in dollars") +  
  ggtitle("10 Highest Property Damages Events") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

propdamagePlot + coord_flip()



 
cropdamagePlot <- ggplot(data=cropdamage, aes(x=reorder(evtype, cropdmgvalue), y=cropdmgvalue, color=evtype)) + 
  geom_bar(stat="identity",fill="white") + 
  xlab("Event Type") + 
  ylab("Total Crop in dollars") +  
  ggtitle("10 Highest Crop Damages Events") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

cropdamagePlot + coord_flip()





