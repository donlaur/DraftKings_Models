source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_class.R")


## Paths to folders containing scraped data

## ------------------------------------------------------------ ##

path.roto.hitters = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Hitters"
path.roto.pitchers = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Pitchers"
path.saber = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Saber_Sim"
path.swish = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Swish_Analytics"
path.nerd = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Fantasy_Nerd"


## Outputs Rotogrinders projection dataframe

## ------------------------------------------------------------ ##


roto.combine = function(path.roto.hitters, path.roto.pitchers) {
  setwd(path.roto.hitters)
  dates = lapply(list.files(), function(x) {
    gsub("[A-z\\.]", "", x)
  })
  hitters.files = list()
  for(file in list.files()) {
    tryCatch ({
      hitters.files = list.append(hitters.files,
                                  clean.rotogrinders(file))
    }, error = function(e) {})
  }
  for(i in 1:length(hitters.files)) {
    hitters.files[[i]]$Date = dates[[i]]
  }
  
  setwd(path.roto.pitchers)
  dates = lapply(list.files(), function(x) {
    gsub("[A-z\\.]", "", x)
  })
  pitchers.files = list()
  for(file in list.files()) {
    tryCatch ({
      pitchers.files = list.append(pitchers.files,
                                  clean.rotogrinders(file))
    }, error = function(e) {})
  }
  for(i in 1:length(pitchers.files)) {
    pitchers.files[[i]]$Date = dates[[i]]
  }
  
  players.files = c(hitters.files, pitchers.files)
  
  roto.df = Reduce(function(x, y) {
    rbind(x, y)
  }, players.files)
  
  return(roto.df)
}


## Outputs SaberSim projection dataframe

## ------------------------------------------------------------ ##


clean.saber = function(path.saber.file) {
  saber.df = read.csv(path.saber.file,
                      stringsAsFactors = F)
}

saber.combine = function(path.saber) {
  setwd(path.saber)
  players.filenames = list.files()
  players.files = list()
  
  for(file in players.filenames) {
    
  }
}
