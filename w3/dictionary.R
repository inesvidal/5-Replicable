dictionary <- function(){
    # initialisation
    mylist.names <- c("key", "terms")
    mylist <- vector("list", length(mylist.names))
    names(mylist) <- mylist.names
    
    
    mylist$terms <- c(
        #Winter Storm Z
        "WIND(.+)SNOW|SNOW(.+)WIND|SNOW(.*)SLEET|FREEZ(.+)RAIN(.+)SNOW|SLEET(.+)ICE|FREEZ|RAIN(.+)SLEET(.+)SNOW|ICE(.+)SNOW|BLOW(.+)SNOW|SNOW(.+)BLOW|BLIZZ(.+)SNOW|ICE(.+)WIND|SNOW(.+)BLIZ|SNOW(.+)SQUALLS|HEAVY SNOW(.+)HIGH WINDS",
        "SNOW/RAIN/SLEET",
        #Thunderstorm Wind C
        "^THUNDERSTORM WIND|^TSTM WIND",
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
        "DRY",
        "DROUGHT",
        #Dust Devil C
        "DEVIL",
        #Dust Storm Z
        "DUST",
        #Excessive Heat Z
        "HEAT",
        #Extreme Cold/Wind Chill Z
        "CHILL | WIND",
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
        #High Wind Z
        "^HIGH WIND",
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
        #Marine High Wind M
        # we include here 2 "MARINE MISHAP" reported with 40mph+ winds, and one "MARINE ACCIDENT" (Fatalities resulting from drowning due to an overturned, damaged, or destroyed marine vessel)
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
    mylist$key <- c(
        #Winter Storm Z
        "WINTER STORM",
        #Thunderstorm Wind C
        "THUNDERSTORM WIND",
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
        #High Wind Z
        "HIGH WIND",
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
    mylist
}
    