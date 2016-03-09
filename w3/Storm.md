---
title: "NOAA Events analysis"
output: html_document
---
## Introduction
In this document we will explore the NOAA Storm database and try to create insights
that can help prevent and reduce the impact of this kind of events.
We will be using the [NOAA database] (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) obtained on Mon Jul 27 00:13:18 2015

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Data processing

Let's load and prepare data: 


```r
#### load data
# variable initialisation
zipfile = "./repdata-data-StormData.csv.bz2"

# set working directory
setwd("/Users/inesv/Coursera/5-Replicable/w3")

# load data, if not done yet
if(!(exists("NOAA"))){
    NOAA <- read.csv(zipfile)
    }
```
We create a new dataframe 'damages' containing only the data referred to personal and material damage and event type.

```r
#### Prepare data 
# Goal understand: which events have highest consequences for humans and for the economy. 
# interesting fields are FATALITIES, INJURIES, PROPDMG, CROPDMG, PROPDMGEXP, CROPDMGEXP
# PROPDMGEXP, CROPDMGEXP need to be interpreted
# From the manual: Estimates should be rounded to three significant digits, 
# followed by an alphabetical character signifying the magnitude of the
# number, i.e., 1.55B for $1,550,000,000. Alphabetical characters used to signify magnitude
# include ???K??? for thousands, ???M??? for millions, and ???B??? for billions. 

## Create two new variables to indicate damage in dollars.
# Organise data
damages <- NOAA %>%
    select(EVTYPE, PROPDMG, CROPDMG, PROPDMGEXP, CROPDMGEXP, FATALITIES, INJURIES)

# Keep only events with economic or personal damage
#damages <- filter(events, !(events$PROPDMG == 0 & events$CROPDMG == 0) | 
#  !(events$FATALITIES == 0 & events$INJURIES))

# Set to uppercase
damages$PROPDMGEXP <- toupper(damages$PROPDMGEXP)
damages$CROPDMGEXP <- toupper(damages$CROPDMGEXP)
damages$EVTYPE <- toupper(damages$EVTYPE)
# remove double spaces + spaces at the beginning or at the end
damages$EVTYPE <- gsub("^ +| +$|( ) +", "\\1", damages$EVTYPE, perl=TRUE)
```

We create a dictionary to associate the different event types to the conventions described in the [NOAA manual] (https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

```r
names <- c("key", "terms")
dic <- vector("character", length(names))
names(dic) <- names

dic$terms <- c(
    #Winter Storm Z
    "WIND(.+)SNOW|SNOW(.+)WIND|SNOW(.*)SLEET|FREEZ(.+)RAIN(.+)SNOW|SLEET(.+)ICE|FREEZ|RAIN(.+)SLEET(.+)SNOW|ICE(.+)SNOW|BLOW(.+)SNOW|SNOW(.+)BLOW|BLIZZ(.+)SNOW|ICE(.+)WIND|SNOW(.+)BLIZ|SNOW(.+)SQUALLS|HEAVY SNOW(.+)HIGH WINDS, SNOW/RAIN/SLEET",
    #Thunderstorm Wind C
    "^THUNDERSTORM WIND|^TSTM WIND",
    #High Wind Z
    "^HIGH(.+)WIND",
    #Astronomical Low Tide Z
    "BLOW-OUT|ASTRONOMICAL LOW TIDE",
    #Avalanche Z
    "AVALANC|DRIFTING SNOW",
    #Blizzard Z
    "BLIZZARD",
    #Coastal Flood Z
    "BEACH|COASTAL FLOOD|HIGH TIDE",
    #Cold/Wind Chill Z
    "COLD WIND CHILL",
    #Debris Flow C
    "DEBRIS",
    #Dense Fog Z
    "^FOG|DENSE FOG|ICE FOG",
    #Dense Smoke Z
    "SMOKE",
    #Drought Z
    "DRY|DROUGHT",
    #Dust Devil C
    "DEVIL",
    #Dust Storm Z
    "DUST",
    #Excessive Heat Z
    "HEAT",
    #Extreme Cold/Wind Chill Z
    "EXTREME(.+)COLD|WIND(.+)CHILL",
    #Flash Flood C
    "FLASH | FLOOD",
    #Flood C
    "FLOOD",
    #Frost/Freeze Z
    "FROST|FREEZE",
    #Funnel Cloud C
    "FUNNEL",
    #Freezing Fog Z
    "FREEZING FOG",
    #Hail C
    "HAIL",
    #Heat Z
    "HEAT",
    #Heavy Rain C
    "HEAVY(.+)RAIN",
    #Heavy Snow Z
    "HEAVY(.+)SNOW",
    #High Surf Z
    "HIGH SURF",
    #Hurricane (Typhoon) Z
    "HURRICANE|TYPHOON",
    #Ice Storm Z
    "ICESTORM",
    #Lake-Effect Snow Z
    "LAKE SNOW|EFFECT SNOW",
    #Lakeshore Flood Z
    "LAKE FLOOD|LAKESHORE FLOOD",
    #Lightning C
    "LIGHTNING",
    #Marine Hail M
    "MARINE HAIL",
    #Marine High Wind M: we include here 2 "MARINE MISHAP" reported with 40mph+ winds, and one "MARINE ACCIDENT" (Fatalities resulting from drowning due to an overturned, damaged, or destroyed marine vessel)
    "MARINE HIGH WIND|MARINE MISHAP|MARINE ACCIDENT",
    #Marine Strong Wind M
    "MARINE STRONG WIND",
    #Marine Thunderstorm Wind M
    "MARINE TSTM WIND|MARINE THUNDERSTORM WIND",
    #Rip Current Z
    "RIP CURRENT",
    #Seiche Z
    "SEICHE",
    #Sleet Z
    "SLEET",
    #Storm Surge/Tide Z
    "SURGE",
    #Strong Wind Z
    "^STRONG WIND",
    #Tornado C
    "TORNADO",
    #Tropical Depression Z
    "TROPICAL DEPRESSION",
    #Tropical Storm Z
    "TROPICAL STORM",
    #Tsunami Z
    "TSUNAMI",
    #Volcanic Ash Z
    "VOLCAN",
    #Waterspout M
    "WATERSPROUT",
    #Wildfire Z
    "WILDFIRE",
    #Winter Weather Z
    "SNOW(.+)COLD")
```

```
## Warning in dic$terms <- c("WIND(.+)SNOW|SNOW(.+)WIND|SNOW(.*)SLEET|FREEZ(.
## +)RAIN(.+)SNOW|SLEET(.+)ICE|FREEZ|RAIN(.+)SLEET(.+)SNOW|ICE(.+)SNOW|BLOW(.
## +)SNOW|SNOW(.+)BLOW|BLIZZ(.+)SNOW|ICE(.+)WIND|SNOW(.+)BLIZ|SNOW(.+)SQUALLS|
## HEAVY SNOW(.+)HIGH WINDS, SNOW/RAIN/SLEET", : Coercing LHS to a list
```

```r
dic$key <- c(
    #Winter Storm Z
    "WINTER STORM",
    #Thunderstorm Wind C
    "THUNDERSTORM WIND",
    #High Wind Z
    "HIGH WIND",
    #Astronomical Low Tide Z
    "ASTRONOMICAL LOW TIDE",
    #Avalanche Z
    "AVALANCHE",
    #Blizzard Z
    "BLIZZARD",
    #Coastal Flood Z
    "COASTAL FLOOD",
    #Cold/Wind Chill Z
    "COLD/WIND CHILL",
    #Debris Flow C
    "DEBRIS FLOW",
    #Dense Fog Z
    "DENSE FOG",
    #Dense Smoke Z
    "DENSE SMOKE",
    #Drought Z
    "DROUGHT",
    #Dust Devil C
    "DUST DEVIL",
    #Dust Storm Z
    "DUST STORM",
    #Excessive Heat Z
    "EXCESSIVE HEAT",
    #Extreme Cold/Wind Chill Z
    "EXTREME COLD/WIND CHILL",
    #Flash Flood C
    "FLASH FLOOD",
    #Flood C
    "FLOOD",
    #Frost/Freeze Z
    "FROST/FREEZE",
    #Funnel Cloud C
    "FUNNEL CLOUD",
    #Freezing Fog Z
    "FREEZING FOG",
    #Hail C
    "HAIL",
    #Heat Z
    "HEAT",
    #Heavy Rain C
    "HEAVY RAIN",
    #Heavy Snow Z
    "HEAVY SNOW",
    #High Surf Z
    "HIGH SURF",
    #Hurricane (Typhoon) Z
    "HURRICANE/TYPHOON",
    #Ice Storm Z
    "ICE STORM",
    #Lake-Effect Snow Z
    "LAKE-EFFECT SNOW",
    #Lakeshore Flood Z
    "LAKESHORE FLOOD",
    #Lightning C
    "LIGHTNING",
    #Marine Hail M
    "MARINE HAIL",
    #Marine High Wind M
    "MARINE HIGH WIND",
    #Marine Strong Wind M
    "MARINE STRONG WIND",
    #Marine Thunderstorm Wind M
    "MARINE THUNDERSTORM WIND",
    #Rip Current Z
    "RIP CURRENT",
    #Seiche Z
    "SEICHE",
    #Sleet Z
    "SLEET",
    #Storm Surge/Tide Z
    "STORM SURGE/TIDE",
    #Strong Wind Z
    "STRONG WIND",
    #Tornado C
    "TORNADO",
    #Tropical Depression Z
    "TROPICAL DEPRESSION",
    #Tropical Storm Z
    "TROPICAL STORM",
    #Tsunami Z
    "TSUNAMI",
    #Volcanic Ash Z
    "VOLCANIC ASH",
    #Waterspout M
    "WATERSPROUT",
    #Wildfire Z
    "WILDFIRE",
    #Winter Weather Z
    "WINTER WEATHER")
```

We unify the different denominations to event types 'EVTYPE' into the new column 'type' using the dictionary above:


```r
# initialise column to flag events already unified
damages$fixed <- FALSE

for (i in 1:length(dic$key)) {
    # find current EVTYPES in dictionary and find corresponding 'key'
    fit_dictionary <- grepl(dic$terms[i], damages$EVTYPE)
    #make sure we don't correct events already fixed
    to_replace <- fit_dictionary & !damages$fixed
    
    damages$type[to_replace] <- toString(dic$key[i])
    
    # update fixed index
    damages$fixed[to_replace] <- TRUE
    }
```


```r
# apply multipliers
multipliers <- data.frame(letter = c("B", "M", "K"), number = c(1e9, 1e6, 1e3)) # faltan el H y los n??meros
# initialise value for damages to property and crops in $
damages$property <- 0
damages$crop <- 0
for ( i in 1:length(multipliers$letter)) {
    prop_units_to_convert <- damages$PROPDMGEXP == multipliers$letter[i]
    damages$property[prop_units_to_convert] <- 
        damages$PROPDMG[prop_units_to_convert] * multipliers$number[i]
    crop_units_to_convert <- damages$CROPDMGEXP == multipliers$letter[i]
    damages$crop[crop_units_to_convert] <- 
        damages$CROPDMG[crop_units_to_convert] * multipliers$number[i]
    }
```

We aggregate damages to property, crops and humans to be able to compare the different event types.


```r
class(damages$type)
```

```
## [1] "character"
```

```r
class(dic$key[1])
```

```
## [1] "character"
```

```r
conclusions <- damages %>%    
    group_by(type) %>%
   summarise(sum_property = sum(property), 
             sum_crop = sum(crop), 
             total_impact = sum(property, crop), 
             sum_deaths = sum(FATALITIES), 
             sum_blessed = sum(INJURIES)) %>%
   arrange(sum_deaths)
```

```r
table <- xtable(conclusions)
print(table, type= "html")
```

```
## <!-- html table generated in R 3.2.1 by xtable 1.7-4 package -->
## <!-- Mon Jul 27 00:17:31 2015 -->
## <table border=1>
## <tr> <th>  </th> <th> type </th> <th> sum_property </th> <th> sum_crop </th> <th> total_impact </th> <th> sum_deaths </th> <th> sum_blessed </th>  </tr>
##   <tr> <td align="right"> 1 </td> <td> ASTRONOMICAL LOW TIDE </td> <td align="right"> 320000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 320000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> </tr>
##   <tr> <td align="right"> 2 </td> <td> COLD/WIND CHILL </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> </tr>
##   <tr> <td align="right"> 3 </td> <td> DEBRIS FLOW </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> </tr>
##   <tr> <td align="right"> 4 </td> <td> DENSE SMOKE </td> <td align="right"> 100000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 100000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> </tr>
##   <tr> <td align="right"> 5 </td> <td> FUNNEL CLOUD </td> <td align="right"> 194600.00 </td> <td align="right"> 0.00 </td> <td align="right"> 194600.00 </td> <td align="right"> 0.00 </td> <td align="right"> 3.00 </td> </tr>
##   <tr> <td align="right"> 6 </td> <td> LAKE-EFFECT SNOW </td> <td align="right"> 40182000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 40182000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> </tr>
##   <tr> <td align="right"> 7 </td> <td> SEICHE </td> <td align="right"> 980000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 980000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> </tr>
##   <tr> <td align="right"> 8 </td> <td> TROPICAL DEPRESSION </td> <td align="right"> 1737000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1737000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> </tr>
##   <tr> <td align="right"> 9 </td> <td> VOLCANIC ASH </td> <td align="right"> 500000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 500000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> </tr>
##   <tr> <td align="right"> 10 </td> <td> FROST/FREEZE </td> <td align="right"> 15000.00 </td> <td align="right"> 108000000.00 </td> <td align="right"> 108015000.00 </td> <td align="right"> 1.00 </td> <td align="right"> 3.00 </td> </tr>
##   <tr> <td align="right"> 11 </td> <td> WINTER WEATHER </td> <td align="right"> 1050000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1050000.00 </td> <td align="right"> 1.00 </td> <td align="right"> 0.00 </td> </tr>
##   <tr> <td align="right"> 12 </td> <td> DUST DEVIL </td> <td align="right"> 719130.00 </td> <td align="right"> 0.00 </td> <td align="right"> 719130.00 </td> <td align="right"> 2.00 </td> <td align="right"> 43.00 </td> </tr>
##   <tr> <td align="right"> 13 </td> <td> SLEET </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 0.00 </td> <td align="right"> 2.00 </td> <td align="right"> 0.00 </td> </tr>
##   <tr> <td align="right"> 14 </td> <td> COASTAL FLOOD </td> <td align="right"> 437141060.00 </td> <td align="right"> 56000.00 </td> <td align="right"> 437197060.00 </td> <td align="right"> 6.00 </td> <td align="right"> 7.00 </td> </tr>
##   <tr> <td align="right"> 15 </td> <td> MARINE HIGH WIND </td> <td align="right"> 1347010.00 </td> <td align="right"> 0.00 </td> <td align="right"> 1347010.00 </td> <td align="right"> 9.00 </td> <td align="right"> 8.00 </td> </tr>
##   <tr> <td align="right"> 16 </td> <td> MARINE STRONG WIND </td> <td align="right"> 418330.00 </td> <td align="right"> 0.00 </td> <td align="right"> 418330.00 </td> <td align="right"> 14.00 </td> <td align="right"> 22.00 </td> </tr>
##   <tr> <td align="right"> 17 </td> <td> MARINE THUNDERSTORM WIND </td> <td align="right"> 5857400.00 </td> <td align="right"> 50000.00 </td> <td align="right"> 5907400.00 </td> <td align="right"> 19.00 </td> <td align="right"> 34.00 </td> </tr>
##   <tr> <td align="right"> 18 </td> <td> WINTER STORM </td> <td align="right"> 58462500.00 </td> <td align="right"> 1889081000.00 </td> <td align="right"> 1947543500.00 </td> <td align="right"> 20.00 </td> <td align="right"> 96.00 </td> </tr>
##   <tr> <td align="right"> 19 </td> <td> DUST STORM </td> <td align="right"> 5619000.00 </td> <td align="right"> 3600000.00 </td> <td align="right"> 9219000.00 </td> <td align="right"> 22.00 </td> <td align="right"> 440.00 </td> </tr>
##   <tr> <td align="right"> 20 </td> <td> STORM SURGE/TIDE </td> <td align="right"> 47965224000.00 </td> <td align="right"> 855000.00 </td> <td align="right"> 47966079000.00 </td> <td align="right"> 24.00 </td> <td align="right"> 43.00 </td> </tr>
##   <tr> <td align="right"> 21 </td> <td> TSUNAMI </td> <td align="right"> 144062000.00 </td> <td align="right"> 20000.00 </td> <td align="right"> 144082000.00 </td> <td align="right"> 33.00 </td> <td align="right"> 129.00 </td> </tr>
##   <tr> <td align="right"> 22 </td> <td> DROUGHT </td> <td align="right"> 1053038600.00 </td> <td align="right"> 13972636780.00 </td> <td align="right"> 15025675380.00 </td> <td align="right"> 38.00 </td> <td align="right"> 48.00 </td> </tr>
##   <tr> <td align="right"> 23 </td> <td> HAIL </td> <td align="right"> 17574568720.00 </td> <td align="right"> 3049437600.00 </td> <td align="right"> 20624006320.00 </td> <td align="right"> 40.00 </td> <td align="right"> 1371.00 </td> </tr>
##   <tr> <td align="right"> 24 </td> <td> TROPICAL STORM </td> <td align="right"> 7714390550.00 </td> <td align="right"> 694896000.00 </td> <td align="right"> 8409286550.00 </td> <td align="right"> 66.00 </td> <td align="right"> 383.00 </td> </tr>
##   <tr> <td align="right"> 25 </td> <td> WILDFIRE </td> <td align="right"> 4865614000.00 </td> <td align="right"> 295972800.00 </td> <td align="right"> 5161586800.00 </td> <td align="right"> 75.00 </td> <td align="right"> 911.00 </td> </tr>
##   <tr> <td align="right"> 26 </td> <td> DENSE FOG </td> <td align="right"> 22829500.00 </td> <td align="right"> 0.00 </td> <td align="right"> 22829500.00 </td> <td align="right"> 81.00 </td> <td align="right"> 1077.00 </td> </tr>
##   <tr> <td align="right"> 27 </td> <td> HEAVY RAIN </td> <td align="right"> 3225571140.00 </td> <td align="right"> 795399800.00 </td> <td align="right"> 4020970940.00 </td> <td align="right"> 98.00 </td> <td align="right"> 255.00 </td> </tr>
##   <tr> <td align="right"> 28 </td> <td> BLIZZARD </td> <td align="right"> 659813950.00 </td> <td align="right"> 112060000.00 </td> <td align="right"> 771873950.00 </td> <td align="right"> 101.00 </td> <td align="right"> 805.00 </td> </tr>
##   <tr> <td align="right"> 29 </td> <td> STRONG WIND </td> <td align="right"> 177674240.00 </td> <td align="right"> 69953500.00 </td> <td align="right"> 247627740.00 </td> <td align="right"> 111.00 </td> <td align="right"> 301.00 </td> </tr>
##   <tr> <td align="right"> 30 </td> <td> HEAVY SNOW </td> <td align="right"> 939854140.00 </td> <td align="right"> 134653100.00 </td> <td align="right"> 1074507240.00 </td> <td align="right"> 127.00 </td> <td align="right"> 1033.00 </td> </tr>
##   <tr> <td align="right"> 31 </td> <td> HURRICANE/TYPHOON </td> <td align="right"> 85356410010.00 </td> <td align="right"> 5516117800.00 </td> <td align="right"> 90872527810.00 </td> <td align="right"> 135.00 </td> <td align="right"> 1333.00 </td> </tr>
##   <tr> <td align="right"> 32 </td> <td> HIGH SURF </td> <td align="right"> 100025000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 100025000.00 </td> <td align="right"> 146.00 </td> <td align="right"> 204.00 </td> </tr>
##   <tr> <td align="right"> 33 </td> <td> AVALANCHE </td> <td align="right"> 3721800.00 </td> <td align="right"> 0.00 </td> <td align="right"> 3721800.00 </td> <td align="right"> 225.00 </td> <td align="right"> 170.00 </td> </tr>
##   <tr> <td align="right"> 34 </td> <td> HIGH WIND </td> <td align="right"> 6003155910.00 </td> <td align="right"> 686301900.00 </td> <td align="right"> 6689457810.00 </td> <td align="right"> 290.00 </td> <td align="right"> 1464.00 </td> </tr>
##   <tr> <td align="right"> 35 </td> <td> EXTREME COLD/WIND CHILL </td> <td align="right"> 78425400.00 </td> <td align="right"> 1313623000.00 </td> <td align="right"> 1392048400.00 </td> <td align="right"> 382.00 </td> <td align="right"> 267.00 </td> </tr>
##   <tr> <td align="right"> 36 </td> <td> FLOOD </td> <td align="right"> 144787844800.00 </td> <td align="right"> 5784026950.00 </td> <td align="right"> 150571871750.00 </td> <td align="right"> 477.00 </td> <td align="right"> 6791.00 </td> </tr>
##   <tr> <td align="right"> 37 </td> <td> RIP CURRENT </td> <td align="right"> 163000.00 </td> <td align="right"> 0.00 </td> <td align="right"> 163000.00 </td> <td align="right"> 577.00 </td> <td align="right"> 529.00 </td> </tr>
##   <tr> <td align="right"> 38 </td> <td>  </td> <td align="right"> 16076401060.00 </td> <td align="right"> 5525609680.00 </td> <td align="right"> 21602010740.00 </td> <td align="right"> 687.00 </td> <td align="right"> 5636.00 </td> </tr>
##   <tr> <td align="right"> 39 </td> <td> THUNDERSTORM WIND </td> <td align="right"> 9760534030.00 </td> <td align="right"> 1224397900.00 </td> <td align="right"> 10984931930.00 </td> <td align="right"> 709.00 </td> <td align="right"> 9468.00 </td> </tr>
##   <tr> <td align="right"> 40 </td> <td> LIGHTNING </td> <td align="right"> 933699280.00 </td> <td align="right"> 12092090.00 </td> <td align="right"> 945791370.00 </td> <td align="right"> 817.00 </td> <td align="right"> 5232.00 </td> </tr>
##   <tr> <td align="right"> 41 </td> <td> FLASH FLOOD </td> <td align="right"> 22307754460.00 </td> <td align="right"> 6595976150.00 </td> <td align="right"> 28903730610.00 </td> <td align="right"> 1042.00 </td> <td align="right"> 1806.00 </td> </tr>
##   <tr> <td align="right"> 42 </td> <td> EXCESSIVE HEAT </td> <td align="right"> 20125750.00 </td> <td align="right"> 904413500.00 </td> <td align="right"> 924539250.00 </td> <td align="right"> 3132.00 </td> <td align="right"> 9209.00 </td> </tr>
##   <tr> <td align="right"> 43 </td> <td> TORNADO </td> <td align="right"> 56993097730.00 </td> <td align="right"> 414961360.00 </td> <td align="right"> 57408059090.00 </td> <td align="right"> 5636.00 </td> <td align="right"> 91407.00 </td> </tr>
##    </table>
```


```r
#Plot results 
# 
# a <- ggplot(data = conclusions, aes(x = sum_crop, 
#                                 y = type)) + 
#     geom_bar() +  
#     xlab("Damage caused") + ylab("Type of event") + ggtitle("Consequences of events") 
# print(a) 
#dev.off();    
```


## Harmfulness

```r
# Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
```

## Economic consequences

```r
## Across the United States, which types of events have the greatest economic consequences?
```

## Results
