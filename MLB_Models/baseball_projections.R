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
  saber.df = subset(saber.df, 
                    select = c(Name, Team, Opponent,
                               Position, Projection,
                               Actual, Price, Value,
                               dk_25_percentile, dk_50_percentile,
                               dk_75_percentile, dk_85_percentile,
                               dk_95_percentile, dk_99_percentile))
  names(saber.df) = c("Name", "Team", "Opponent",
                      "Position", "Projection", "Actual",
                      "Salary", "Value", "Projection_25", 
                      "Projection_50", "Projection_75", 
                      "Projection_85", "Projection_95",
                      "Projection_99")
  saber.df$Team = toupper(saber.df$Team)
  saber.df$Opponent = toupper(saber.df$Opponent)
  return(saber.df)
}

saber.combine = function(path.saber) {
  setwd(path.saber)
  players.filenames = list.files()
  players.filenames = players.filenames[grepl("sabersim", players.filenames)]
  players.files = list()
  dates = lapply(players.filenames, function(x) {
    unlist(strsplit(gsub("[A-z\\.]", "", x), split = " "))[1]
  })
  
  for(file in players.filenames) {
    tryCatch ({
      players.files = list.append(players.files,
                                  clean.saber(file))
    }, error = function(e) {})
  }
  
  for(i in 1:length(players.files)) {
    players.files[[i]]$Date = dates[[i]]
  }
  
  saber.df = Reduce(function(x, y) {
    rbind(x, y)
  }, players.files)
  
  return(saber.df[-which(duplicated(saber.df)),])
}

## Outputs Swish Analytics projection dataframe

## ------------------------------------------------------------ ##

clean.swish = function(path.swish.file) {
  swish.df = read.csv(path.swish.file,
                      stringsAsFactors = F,
                      header = F)
  names(swish.df) = c("Position",
                      "Name",
                      "Salary",
                      "Projection",
                      "Teams.Playing")
  swish.df$Salary = gsub(c("\\$"), "", swish.df$Salary)
  swish.df$Salary = gsub(c(","), "", swish.df$Salary)
  swish.df$Salary = as.numeric(swish.df$Salary)
  swish.df$Teams.Playing = gsub(" ", "", swish.df$Teams.Playing)
  games = strsplit(swish.df$Teams.Playing, split = "@")
  swish.df$Team = "CHC"
  swish.df$Opponent = "CHC"
  for(i in 1:nrow(swish.df)) {
    swish.df[i,]$Team = games[[i]][1]
    swish.df[i,]$Opponent = games[[i]][2]
  }
  return(swish.df)
}

swish.combine = function(path.swish) {
  setwd(path.swish)
  players.filenames = list.files()
  players.filenames = players.filenames[grepl("swish", players.filenames)]
  players.files = list()
  dates = lapply(players.filenames, function(x) {
    unlist(strsplit(gsub("[A-z\\.]", "", x), split = " "))[1]
  })
  
  for(file in players.filenames) {
    tryCatch ({
      players.files = list.append(players.files,
                                  clean.swish(file))
    }, error = function(e) {})
  }
  
  for(i in 1:length(players.files)) {
    players.files[[i]]$Date = dates[[i]]
  }
  
  swish.df = Reduce(function(x, y) {
    rbind(x, y)
  }, players.files)
  
  return(swish.df)
}


## Outputs Daily Fantasy Nerd projection dataframe

## ------------------------------------------------------------ ##


