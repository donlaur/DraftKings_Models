## This file contains code which cleans Draftkings, Rotogrinders,
## and ESPN CSV files. These functionalities are stored here because
## they are currently not being used in the algorithm.
##
## ------------------------------------------------------------ ##


library(magrittr)
library(rlist)
library(dplyr)
library(purrr)
library(DescTools)

# Path to read Draftkings player CSV file from
path.draftkings = "C:/Users/Ming/Documents/Fantasy_Models/data/DRAFT_07282018.csv"

# Path to read Rotogrinders player CSV files from
path.roto.pitchers = "C:/Users/Ming/Documents/Fantasy_Models/data/ROTO_HITTERS_07282018.csv"
path.roto.hitters = "C:/Users/Ming/Documents/Fantasy_Models/data/ROTO_HITTERS_07282018.csv"

# Path to read comprehensive ESPN MLB player data from
path.ESPN = "C:/Users/Ming/Documents/Fantasy_Models/MLB_data"

## Read Draftkings CSV file and modify columns to create a clean dataframe
## This function is not currently used in the code, as the Rotogrinder's
## CSV file provides the same information as the Draftkings CSV file,
## with the inclusion of player score projections.

## ------------------------------------------------------------ ##


read.draftkings = function(path.draftkings) {
  # Read raw CSV file
  players = read.csv(path.draftkings, stringsAsFactors = F)
  
  # Change column names
  names(players) = c("Roster.Position", "NameID", "Name", 
                     "ID", "Position",
                     "Salary", "Teams.Playing",
                     "Team", "Projection")
  
  # Remove unnecessary columns
  players = select(players, 
                   subset = -c(Name, ID, Roster.Position))
  
  teams.playing = unlist(lapply(players$Teams.Playing, 
                                function(x) {
                                  unlist(strsplit(x, split = " "))[[1]]
                                }), 
                         use.names = F)
  teams.playing = lapply(teams.playing,
                         function(x) {
                           unlist(strsplit(x, split = "@"))
                         })
  
  own.team = players$Team
  opponents = c()
  for(i in 1:length(teams.playing)) {
    opponents = append(opponents, 
                       teams.playing[[i]][teams.playing[[i]] != own.team[i]])
  }
  players$Opponent = opponents
  hitters = players[players$Position != "P",]
  pitchers = players[players$Position == "P",]
  
  return(list(hitters = hitters,
              pitchers = pitchers))
}


## Read Rotogrinders CSV file and modify columns to create a clean dataframe

## ------------------------------------------------------------ ##


read.rotogrinders = function(path.roto.hitters, path.roto.pitchers) {
  hitters = read.csv(path.roto.hitters,
                     stringsAsFactors = F,
                     header = F)
  
  pitchers = read.csv(path.roto.pitchers,
                      stringsAsFactors = F,
                      header = F)
  
  players = list(hitters = hitters,
                 pitchers = pitchers)
  
  for(i in 1:length(players)) {
    colnames(players[[i]]) = c("Name", 
                               "Salary",
                               "Team",
                               "Position",
                               "Opponent",
                               "None",
                               "Percentage",
                               "Projection")
    players[[i]] = select(players[[i]],
                          subset = -c(None, Percentage))
    players[[i]]$Teams.Playing = apply(players[[i]][,c("Team", "Opponent")], 
                                       1, 
                                       paste, 
                                       collapse = "@")
  }
  return(players)
}

players = read.rotogrinders(path.roto.hitters, 
                            path.roto.pitchers)

hitters = players[["hitters"]]
pitchers = players[["pitchers"]]


## Read ESPN CSV file and create two lists: one for hitters' dataframes, 
## and one for pitchers' dataframes, where each dataframe 
## contains a Draftkings Fantasy Points column calculated 
## from their game-by-game ESPN stats

## ------------------------------------------------------------ ##


convert_result = function(result){
  return(ifelse(grepl("W", result), 1, 0))
}

shutout = function(runs){
  return(ifelse(runs == 0, 1, 0))
}

complete_game = function(innings){
  return(ifelse(innings == 9, 1, 0))
}

nohitter = function(hits){
  return(ifelse(hits == 0, 1, 0))
}

setwd(path.ESPN)
pitcher.list = list()
hitter.list = list()

for(file in list.files()) {
  # Read the csv file
  df = read.csv(file, stringsAsFactors = F,
                check.names = F)
  
  # Get rid of games where the player didn't play
  df = df[complete.cases(df), ]
  
  tryCatch ({
    # Determine whether the player is a pitcher or hitter
    if ("IP" %in% colnames(df)) {
      # Make result (win/loss) column into factor
      df$RESULT = sapply(df$RESULT, convert_result)
      df$Dec. = sapply(df$Dec., convert_result)
      df$Shutout = sapply(df$R, shutout)
      df$Complete.Game = sapply(df$IP, complete_game)
      df$No.Hitter = sapply(df$H, nohitter)
      
      df$Points = 2.25 * df[,"IP"] + 2 * df[,"SO"] + 
        4 * df[,"Dec."] - 2 * df[,"ER"] - 
        0.6 * (df[,"H"] + df[,"BB"]) + 2.5 * df[,"Complete.Game"] + 
        2.5 * df[,"Complete.Game"] * df[,"Shutout"] + 
        5 * df[,"Complete.Game"] * df[,"No.Hitter"]
      
      pitcher.list = list.append(pitcher.list, file = df)
      names(pitcher.list)[which(names(pitcher.list) == "file")] = file
    }
    else {
      df$Points = 3 * df[,"H"] + 5 * df[,"2B"] +
        8 * df[,"3B"] + 10 * df[,"HR"] +
        2 * df[,"RBI"] + 2 * df[,"R"] +
        2 * df[,"BB"] + 5 * df[,"SB"]
      hitter.list = list.append(hitter.list, file = df)
      names(hitter.list)[which(names(hitter.list) == "file")] = file
    }
  }, error = function(e) {})
}


## Merge ESPN and Rotogrinders CSV files, including a column for
## player variance

## ------------------------------------------------------------ ##


fetch.files = function(espn.list,
                       player.profile) {
  name = unlist(strsplit(player.profile$Name, split = " "))
  index = 0
  for(df in names(espn.list)) {
    if(all(sapply(name, function(x) {
      grepl(x, df, ignore.case = T) 
    }))) {
      index = which(names(espn.list) == df)
    }
  }
  if(index == 0) {
    name = paste(name, collapse = " ")
    for(df in names(espn.list)) {
      csv.file = gsub(".csv", "", df)
      csv.file = gsub("\\.-", "\\.", csv.file)
      csv.file = gsub("-", "\\.", csv.file)
      csv.file = unlist(strsplit(csv.file, split = "\\."))
      if(all(sapply(csv.file, function(x) {
        grepl(x, name, ignore.case = T) 
      }))) {
        index = which(names(espn.list) == df)
      }
    }
  }
  tryCatch ({
    player.df = espn.list[[index]]
    return(player.df)
  }, error = function(e) {})
  return(NA)
}

get.sd = function(espn.list, roto.file) {
  roto.file$Sigma = NA
  
  for(i in 1:nrow(roto.file)) {
    tryCatch ({
      df = fetch.files(espn.list, roto.file[i,])
      roto.file[i,]$Sigma = sd(df$Points)
    }, error = function(e) {})
  }
  
  return(roto.file)
}

hitters = get.sd(hitter.list, hitters)
pitchers = get.sd(pitcher.list, pitchers)


## Create covariance matrix for hitters

## ------------------------------------------------------------ ##


get.cov = function(espn.list, 
                   roto.file) {
  width = nrow(roto.file)
  covariances = matrix(rep(0, width^2), nrow = width)
  
  for(i in 1:width) {
    for(j in 1:width) {
      if(i == j | roto.file[i,]$Team != roto.file[j,]$Team) {
        covariances[i, j] = 0
      }
      else {
        player.1 = fetch.files(espn.list, roto.file[i,])
        player.2 = fetch.files(espn.list, roto.file[j,])
        tryCatch ({
          player.1 = player.1[,c("DATE","Points")]
          player.2 = player.2[,c("DATE","Points")]
          merged.players = merge(player.1,
                                 player.2,
                                 by = "DATE")
          covariances[i, j] = cov(merged.players$Points.x,
                                  merged.players$Points.y)
        }, error = function(e) {})
      }
    }
  }
  
  return(covariances)
}