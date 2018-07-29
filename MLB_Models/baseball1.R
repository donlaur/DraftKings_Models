## Required libraries

## ------------------------------------------------------------ ##


library(ompr)
library(magrittr)
library(ROI)
library(ompr.roi)
library(rlist)
library(ROI.plugin.symphony)
library(ROI.plugin.glpk)
library(dplyr)
library(purrr)
library(DescTools)


## Macro hockey variables: change these

## ------------------------------------------------------------ ##


# Number of lineups we wish to generate
# Note: Each additional lineup will take longer to
#       generate than the last 
num.lineups = 20

# Maximum overlap of players among lineups
num.overlap = 5

# Salary cap (dollars)
salary.cap = 50000


## Path to CSV files: change these

## ------------------------------------------------------------ ##


# Path to write output file to on personal computer
path.output = "C:/Users/Ming/Documents/Fantasy_Models/output/MLB_lineup.csv"

# Path to read Draftkings player CSV file from
path.draftkings = "C:/Users/Ming/Documents/Fantasy_Models/data/DRAFT_07282018.csv"

# Path to read Rotogrinders player CSV files from
path.roto.pitchers = "C:/Users/Ming/Documents/Fantasy_Models/data/ROTO_PITCHERS_07282018.csv"
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
          covariances[i, j] = cor(merged.players$Points.x,
                                  merged.players$Points.y)
        }, error = function(e) {})
      }
    }
  }
  
  return(covariances)
}


## Create one lineup using integer linear programming

## ------------------------------------------------------------ ##


nonstacked.lineup = function(hitters, pitchers, lineups, num.overlap,
                             num.hitters, num.pitchers, first.basemen,
                             second.basemen, third.basemen,
                             shortstops, outfielders,
                             catchers, num.teams, hitters.teams,
                             num.games, hitters.games, pitchers.games,
                             salary.cap, players.variance, hitters.covariance, 
                             pitchers.opponents) {
  w = function(i, j) {
    vapply(seq_along(i), function(k) hitters.covariance[i[k], j[k]], numeric(1L))
  }
  
  m = MILPModel() %>%
    # Vector consisting of dummy variables indicating whether each skater is chosen for the lineup
    add_variable(hitters.lineup[i],
                 i = 1:num.hitters,
                 type = "binary") %>%
    
    # Vector consisting of dummy variables indicating whether each goalie is chosen
    add_variable(pitchers.lineup[i],
                 i = 1:num.pitchers,
                 type = "binary") %>%
    
    # Vector consisting of dummy variables indicating whether a team is represented in the hitter lineup
    add_variable(used.team[i],
                 i = 1:num.teams,
                 type = "binary") %>%
    
    # Eight hitters constraint
    add_constraint(sum_expr(hitters.lineup[i], i = 1:num.hitters) == 8) %>%
    
    # Two pitchers constraint
    add_constraint(sum_expr(pitchers.lineup[i], i = 1:num.pitchers) == 2) %>%
    
    # Variance lower bound
    # add_constraint(sum_expr(colwise(w(i,j)) * hitters.lineup[j], i = 1:num.hitters, j = 1:num.hitters) +
    #                  sum_expr(colwise(players.variance[i]) * hitters.lineup[i], i = 1:num.hitters) +
    #                  sum_expr(colwise(players.variance[num.hitters + j]) * pitchers.lineup[j], j = 1:num.pitchers) >= 10) %>%
                     
    # One of each position besides outfielders
    add_constraint(sum_expr(colwise(first.basemen[i]) * hitters.lineup[i], i = 1:num.hitters) == 1) %>%
    add_constraint(sum_expr(colwise(second.basemen[i]) * hitters.lineup[i], i = 1:num.hitters) == 1) %>%
    add_constraint(sum_expr(colwise(third.basemen[i]) * hitters.lineup[i], i = 1:num.hitters) == 1) %>%
    add_constraint(sum_expr(colwise(shortstops[i]) * hitters.lineup[i], i = 1:num.hitters) == 1) %>%
    add_constraint(sum_expr(colwise(catchers[i]) * hitters.lineup[i], i = 1:num.hitters) == 1) %>%
    
    # Players who play two positions cannot be chosen twice
    add_constraint(sum_expr(colwise(first.basemen[i]) * hitters.lineup[i] +
                              colwise(second.basemen[i]) * hitters.lineup[i] +
                              colwise(third.basemen[i]) * hitters.lineup[i] +
                              colwise(shortstops[i]) * hitters.lineup[i] +
                              colwise(catchers[i]) * hitters.lineup[i] +
                              colwise(outfielders[i]) * hitters.lineup[i], 
                            i = 1:num.hitters) <= num.hitters) %>%
    
    # Budget constraint
    add_constraint(sum_expr(colwise(hitters[i, "Salary"]) * hitters.lineup[i], i = 1:num.hitters) +
                     sum_expr(colwise(pitchers[i, "Salary"]) * pitchers.lineup[i], i = 1:num.pitchers) <= salary.cap)
  
  # No more than five hitters from the same team
  for(i in 1:num.teams) {
    m = add_constraint(m, used.team[i] <= sum_expr(colwise(hitters.teams[t,i]) * hitters.lineup[t], t = 1:num.hitters))
    m = add_constraint(m, sum_expr(colwise(hitters.teams[t,i]) * hitters.lineup[t], t = 1:num.hitters) <= 5 * used.team[i])
  }
  m = add_constraint(m, sum_expr(used.team[i], i = 1:num.teams) >= 2)
  
  # Players must come from at least two different games
  m = add_variable(m, used.game[i], i = 1:num.games, type = "binary")
  
  for(i in 1:num.games) {
    m = add_constraint(m, used.game[i] <= 
                         sum_expr(colwise(hitters.games[t,i]) * hitters.lineup[t], t = 1:num.hitters) +
                         sum_expr(colwise(pitchers.games[t,i]) * pitchers.lineup[t], t = 1:num.pitchers))
  }
  m = add_constraint(m, sum_expr(used.game[i], i = 1:num.games) >= 2)

  # Pitcher constraint: no pitcher and hitter can come from the same team
  for(i in 1:num.pitchers) {
    m = add_constraint(m, sum_expr(5 * pitchers.lineup[i]) +
                         sum_expr(colwise(pitchers.opponents[i, j]) * hitters.lineup[j], j = 1:num.hitters) <= 5)
  }
  
  # Overlap constraint
  for(i in 1:nrow(lineups)) {
    m = add_constraint(m, sum_expr(colwise(lineups[i,j]) * hitters.lineup[j], j = 1:num.hitters) +
                         sum_expr(colwise(lineups[i, num.hitters + j]) * pitchers.lineup[j], j = 1:num.pitchers) <= num.overlap)
  }
  
  m = set_objective(m,
                    sum_expr(colwise(hitters[i, "Projection"]) * hitters.lineup[i], i = 1:num.hitters) +
                      sum_expr(colwise(pitchers[i, "Projection"]) * pitchers.lineup[i], i = 1:num.pitchers),
                    sense = "max")
  result = solve_model(m, with_ROI(solver = "symphony"))
  print(result)
  hitters.df = get_solution(result, hitters.lineup[i])
  pitchers.df = get_solution(result, pitchers.lineup[i])
  return(append(hitters.df[, "value"], pitchers.df[, "value"]))
}

create_lineups = function(num.lineups, num.overlap, formulation, salary.cap,
                          hitters, pitchers) {
  
  # Number of hitters
  num.hitters = nrow(hitters)
  
  # Number of pitchers
  num.pitchers = nrow(pitchers)
  
  # Create a matrix where each row is a vector for a different position
  empty = rep(0, num.hitters * 6)
  
  hitters.matrix = matrix(empty, nrow = 6)
  
  # Populate vectors for all positions
  for(i in 1:num.hitters) {
    if(grepl("1B", hitters[i, "Position"])) {
      hitters.matrix[1, i] = 1
    }
    if(grepl("2B", hitters[i, "Position"])) {
      hitters.matrix[2, i] = 1
    }
    if(grepl("3B", hitters[i, "Position"])) {
      hitters.matrix[3, i] = 1
    }
    if(grepl("SS", hitters[i, "Position"])) {
      hitters.matrix[4, i] = 1
    }
    if(grepl("OF", hitters[i, "Position"])) {
      hitters.matrix[5, i] = 1
    }
    if(grepl("C", hitters[i, "Position"])) {
      hitters.matrix[6, i] = 1
    }
  }
  hitters.list = list(first.basemen = hitters.matrix[1,],
                      second.basemen = hitters.matrix[2,],
                      third.basemen = hitters.matrix[3,],
                      shortstops = hitters.matrix[4,],
                      outfielders = hitters.matrix[5,],
                      catchers = hitters.matrix[6,])
  
  # List where each entry is a vector that stores info on what team a player is on
  teams = unique(hitters[,"Team"])
  num.teams = length(teams)
  
  team.distribution = list()
  
  for(i in 1:num.hitters) {
    player.info = rep(0, num.teams)
    for(j in 1:num.teams) {
      if(hitters[i, "Team"] == teams[j]) {
        player.info[j] = 1
      }
    }
    team.distribution = list.append(team.distribution, player.info)
  }
  
  # Converting list into a matrix
  hitters.teams = Reduce(function(x, y) {
    rbind(x, y)
  }, team.distribution)
  
  # List where each entry is a vector that stores what game a hitter is playing in
  games = unique(hitters[,"Teams.Playing"])
  num.games = length(games)
  
  games.distribution = list()
  
  for(i in 1:num.hitters) {
    player.info = rep(0, num.games) 
    for(j in 1:num.games) {
      if(hitters[i, "Teams.Playing"] == games[j]) {
        player.info[j] = 1
      }
    }
    games.distribution = list.append(games.distribution, player.info)
  }
  
  # Converting list into a matrix
  hitters.games = Reduce(function(x, y) {
    rbind(x, y)
  }, games.distribution)
  
  # List where each entry is a vector that stores what game a pitcher is playing in
  games = unique(pitchers[,"Teams.Playing"])
  num.games = length(games)
  
  games.distribution = list()
  
  for(i in 1:num.pitchers) {
    player.info = rep(0, num.games) 
    for(j in 1:num.games) {
      if(pitchers[i, "Teams.Playing"] == games[j]) {
        player.info[j] = 1
      }
    }
    games.distribution = list.append(games.distribution, player.info)
  }
  
  # Converting list into a matrix
  pitchers.games = Reduce(function(x, y) {
    rbind(x, y)
  }, games.distribution)
  
  print("Initializing ROI.plugin.symphony...")
  print("Generating lineups (this may take a while)...")
  
  # Mock variance vector
  players.variance = append(hitters$Sigma, pitchers$Sigma)
  
  # Covariance matrix
  hitters.covariance = get.cov(hitter.list, hitters)
  
  # Pitchers' opponents
  opponents = pitchers[,"Opponent"]
  
  opponents.list = list()
  for(i in 1:length(opponents)) {
    for(j in 1:num.teams) {
      if(opponents[i] == teams[j]) {
        opponents.list = list.append(opponents.list, hitters.teams[,j])
      }
    }
  }
  
  pitchers.opponents = Reduce(function(x, y) {
    rbind(x, y)
  }, opponents.list)
  
  # Create a lineup
  tracer = matrix(rep(0, num.hitters + num.pitchers), nrow = 1)
  lineups = formulation(hitters, pitchers, tracer, num.overlap,
                        num.hitters, num.pitchers, hitters.list[[1]],
                        hitters.list[[2]], hitters.list[[3]],
                        hitters.list[[4]], hitters.list[[5]],
                        hitters.list[[6]], num.teams, hitters.teams,
                        num.games, hitters.games, pitchers.games,
                        salary.cap, players.variance, hitters.covariance, 
                        pitchers.opponents)
  lineups = matrix(lineups, nrow = 1)
  
  for(i in 1:(num.lineups - 1)) {
    lineup = formulation(hitters, pitchers, lineups, num.overlap,
                         num.hitters, num.pitchers, hitters.list[[1]],
                         hitters.list[[2]], hitters.list[[3]],
                         hitters.list[[4]], hitters.list[[5]],
                         hitters.list[[6]], num.teams, hitters.teams,
                         num.games, hitters.games, pitchers.games,
                         salary.cap, players.variance, hitters.covariance, 
                         pitchers.opponents)
    lineups = rbind(lineups, lineup)
  }
  
  print("Lineups successfully generated!")
  
  return(lineups)
}

lineups.to.csv = function(lineups, hitters, pitchers, path.output) {
  
  # Writing to CSV file
  for(i in 1:nrow(lineups)) {
    lineup = lineups[i,]
    chosen.hitters = lineup[1:nrow(hitters)]
    chosen.pitchers = lineup[(nrow(hitters) + 1):length(lineup)]
    
    hitters.indices = which(chosen.hitters == 1)
    pitchers.indices = which(chosen.pitchers == 1)
    
    names = append(c(hitters[hitters.indices, "NameID"]),
                   c(pitchers[pitchers.indices, "NameID"]))
    write.table(matrix(names, nrow = 1),
                path.output,
                sep = ",",
                append = T,
                col.names = F,
                row.names = F)
  }
  
  print(paste("CSV successfully exported to ", path.output, sep = " "))
}


## Create lineups ##

## ------------------------------------------------------------ ##


df = create_lineups(num.lineups, num.overlap, nonstacked.lineup, salary.cap, hitters, pitchers)


## Write to CSV ##

## ------------------------------------------------------------ ##


lineups.to.csv(df, hitters, pitchers, path.output)