source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_class.R")


## Paths to folders containing scraped data

## ------------------------------------------------------------ ##

path.roto.hitters = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Hitters"
path.roto.pitchers = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Pitchers"
path.sabersim = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Saber_Sim"
path.swish = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Swish_Analytics"
path.nerd = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Fantasy_Nerd"


## Outputs Rotogrinders projection dataframe

## ------------------------------------------------------------ ##


roto.combine = function(path.roto.hitters, path.roto.pitchers) {
  setwd(path.roto.hitters)
  hitters.files = lapply(list.files(), clean.rotogrinders)
  
  setwd(path.roto.pitchers)
  pitchers.files = lapply(list.files(), clean.rotogrinders)
  
  players.files = list.append(hitters.files, pitchers.files)
  
  roto.df = Reduce(function(x, y) {
    rbind(x, y)
  }, players.files)
  
  return(roto.df)
}


## Outputs Sabersim projection dataframe

## ------------------------------------------------------------ ##


