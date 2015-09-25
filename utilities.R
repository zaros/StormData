# mapEvents()
# Maps various erroneous event type data (miscategorisation, spelling errors, etc.)
# from Storm Data to the valid event types

mapEvents <- function(mapto) {
  mapto <- gsub("^TSTM WIND","THUNDERSTORM WIND",mapto)
  mapto <- gsub("^TSTMW","THUNDERSTORM WIND",mapto)

  mapto <- gsub("^THUNDERSTORM.*","THUNDERSTORM WIND",mapto)
#  mapto <- gsub("^THUNDERSTORM WIND.*","THUNDERSTORM WIND",mapto)
 # mapto <- gsub("^THUNDERSTORMW.*","THUNDERSTORM WIND",mapto)
 # mapto <- gsub("^THUNDERSTORMS WIND.*","THUNDERSTORM WIND",mapto)
  mapto <- gsub("^(THUNDERTORM|THUNDEERSTORM|THUNDERESTORM|THUNDERSTORM|THUNERSTORM|THUDERSTORM|THUNDERSTROM|TUNDERSTORM) (WIND.*|WINS)$","THUNDERSTORM WIND",mapto)
  mapto <- gsub("^SEVERE THUNDERSTORM.*","THUNDERSTORM WIND",mapto)
  mapto <- gsub("^STORM FORCE.*","THUNDERSTORM WIND",mapto)

  mapto <- gsub("^WHIRLWIND$","THUNDERSTORM WIND",mapto)

  mapto <- gsub("^MARINE.*WIND.*","MARINE THUNDERSTORM WIND",mapto)
  mapto <- gsub("^(HURRICANE|TYPHOON).*","HURRICANE (TYPHOON)",mapto)
  mapto <- gsub("^TORNADO.*","TORNADO",mapto)
  mapto <- gsub("^TORNDAO","TORNADO",mapto)

mapto <- gsub("^WATERSPOUT.*","WATERSPOUT",mapto)
  mapto <- gsub("^LIGHTNING.*","LIGHTNING",mapto)
mapto <- gsub("^(LIGHTING|LIGNTNING)","LIGHTNING",mapto)
  mapto <- gsub("^AVALANCE.*","AVALANCHE",mapto)
  mapto <- gsub("^WILD.*|BRUSH FIRE|GRASS FIRE.*","WILDFIRE",mapto)
  mapto <- gsub("^HIGH WIND.*","HIGH WIND",mapto)
  mapto <- gsub("^RIP .*","RIP CURRENT",mapto)
  mapto <- gsub("^FLASH FLOOD.*","FLASH FLOOD",mapto)


  mapto <- gsub("^FLOOD.*|HIGH WATER|RIVER FLOODING|RIVER FLOOD|MAJOR FLOOD","FLOOD",mapto)
  mapto <- gsub("^UNSEASONABLY WARM.*","HEAT",mapto)

  mapto <- gsub("^(EXTREME|RECORD|EXCESSIVE) HEAT|HEAT WAVE.*","EXCESSIVE HEAT",mapto)
  mapto <- gsub("^RECORD/EXCESSIVE HEAT","EXCESSIVE HEAT",mapto)

  mapto <- gsub("^(EXTREME|EXCESSIVE|RECORD) (WINDCHILL|COLD).*","EXTREME COLD/WIND CHILL",mapto)
  mapto <- gsub("^(EXTENDED|UNSEASONABLY) COLD.*","COLD/WINDCHILL",mapto)
  mapto <- gsub("^LOW TEMPERATURE","COLD/WINDCHILL",mapto)

  mapto <- gsub("^COLD.*","COLD/WIND CHILL",mapto)
  mapto <- gsub("^TROPICAL STORM.*","TROPICAL STORM",mapto)
  mapto <- gsub("^COASTAL FLOOD.*|TIDAL FLOODING","COASTAL FLOOD",mapto)
  mapto <- gsub("^COASTALSTORM.*","COASTAL STORM",mapto)

  mapto <- gsub("^DROUGHT.*","DROUGHT",mapto)
  mapto <- gsub("^(HEAVY|HIGH|ROUGH|HAZARDOUS) (SEAS|WAVES|SWELLS|SURF).*","HIGH SURF",mapto)

  mapto <- gsub("^WINTER WEATHER.*","WINTER WEATHER",mapto)
  mapto <- gsub("^WINTER STORM.*","WINTER STORM",mapto)
  mapto <- gsub("^ICE STORM.*","-ICE STORM",mapto)
  mapto <- gsub("^GLAZE.*","-FREEZING FOG",mapto)
  mapto <- gsub("^(FROST|FREEZE|ICE|ICY|FREEZING|BLACK ICE).*","FROST/FREEZE",mapto)
  mapto <- gsub("^-ICE STORM","ICE STORM",mapto)
  mapto <- gsub("^-FREEZING FOG","FREEZING FOG",mapto)

  mapto <- gsub("^(HEAVY|EXCESSIVE|TORRENTIAL|RECORD) (RAIN|PRECIPITATION|SHOWER).*","HEAVY RAIN",mapto)
  mapto <- gsub("^(HEAVY|EXCESSIVE|RECORD) SNOW.*","HEAVY SNOW",mapto)
  mapto <- gsub("^(STRONG|GUSTY) WIND.*","STRONG WIND",mapto)

  mapto <- gsub("^(LANDSLIDE|MUDSLIDE|MUD SLIDE|ROCK SLIDE).*","DEBRIS FLOW",mapto)
  mapto <- gsub("^FOG.*","DENSE FOG",mapto)
  mapto <- gsub(".*HAIL.*","HAIL",mapto)
  mapto <- gsub("STORM SURGE.*","STORM SURGE/TIDE",mapto)

  # return the mapped vector
  mapto
}

# calcExps()
# returns a vector of calculated value & exponent pairs
# the substitutions for values of exponents column is based
# on this report:
# https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html

calcExps <- function(values,exps) {
  result <- numeric(length(values))

  for (i in seq_along(values)) {

    exp <- switch(toupper(exps[i]),
                   K=10^3,
                   M=10^6,
                   H=10^2,
                   B=10^9,
                   "+"=1,
                  "1"=10,
                  "2"=10,
                  "3"=10,
                  "4"=10,
                  "5"=10,
                  "6"=10,
                  "7"=10,
                  "8"=10,
                   0 # default
                   )

    result[i] <- values[i]*exp
  }
  result
}
