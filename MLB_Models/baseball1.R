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
path.players = "C:/Users/Ming/Documents/Fantasy_Models/data/MLB_07212018.csv"

# Path to read comprehensive MLB player data from
path.MLB = "C:/Users/Ming/Documents/Fantasy_Models/MLB_data"

## Read Draftkings CSV file and modify columns to create a clean dataframe

## ------------------------------------------------------------ ##


read.file = function(path.players) {
  # Read raw CSV file
  players = read.csv(path.players, stringsAsFactors = F)
  
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
  return(players)
}

# Cleaned CSV file
players = read.file(path.players)

# Split players CSV file into two files:
# one for hitters and one for pitchers

hitters = players[players$Position != "P",]
pitchers = players[players$Position == "P",]


## Path to folder containing game-by-game data on all MLB players

## ------------------------------------------------------------ ##


setwd(path.MLB)
file.list = list()
for(file in list.files()) {
  read.file = read.csv(file, stringsAsFactors = F) %>% complete.cases()
  # FOR DYLAN <3 #
}

nonstacked.lineup = function(hitters, pitchers, lineups, num.overlap,
                             num.hitters, num.pitchers, first.basemen,
                             second.basemen, third.basemen,
                             shortstops, outfielders,
                             catchers, num.teams, hitters.teams,
                             num.games, hitters.games, pitchers.games,
                             salary.cap, players.variance, players.covariance,
                             hitters.covariance, pitchers.opponents) {
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
    add_constraint(sum_expr(colwise(w(i,j)) * hitters.lineup[j], i = 1:num.hitters, j = 1:num.hitters) +
                     sum_expr(colwise(players.variance[i]) * hitters.lineup[i], i = 1:num.hitters) +
                     sum_expr(colwise(players.variance[num.hitters + j]) * pitchers.lineup[j], j = 1:num.pitchers) >= 10) %>%
                     
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
  players.variance = runif(num.hitters + num.pitchers)
  
  # Mock covariance matrix
  players.covariance = matrix(runif((num.hitters + num.pitchers)^2), 
                              nrow = (num.hitters + num.pitchers))
  
  hitters.covariance = matrix(runif(num.hitters^2), nrow = num.hitters)
  
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
                        salary.cap, players.variance, players.covariance,
                        hitters.covariance, pitchers.opponents)
  lineups = matrix(lineups, nrow = 1)
  
  for(i in 1:(num.lineups - 1)) {
    lineup = formulation(hitters, pitchers, lineups, num.overlap,
                         num.hitters, num.pitchers, hitters.list[[1]],
                         hitters.list[[2]], hitters.list[[3]],
                         hitters.list[[4]], hitters.list[[5]],
                         hitters.list[[6]], num.teams, hitters.teams,
                         num.games, hitters.games, pitchers.games,
                         salary.cap, players.variance, players.covariance,
                         hitters.covariance, pitchers.opponents)
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