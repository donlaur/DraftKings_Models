## This file contains legacy code (in case it once again becomes useful).
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

backtest.optimal = function(salary.cap,
                            num.lineups, path.hitters.proj,
                            path.pitchers.proj, path.players.actual,
                            year, month, days) {
  num.days = days[[toString(month)]]
  optimum.teams = list()
  optimum.points = list()
  for(day in 14:(num.days-15)) {
    path.hitters.proj.temp = gsub.custom(path.hitters.proj, year, month, day)
    path.pitchers.proj.temp = gsub.custom(path.pitchers.proj, year, month, day)
    path.players.actual.temp = gsub.custom(path.players.actual, year, month, day)
    
    hitters.proj = clean.rotogrinders(path.hitters.proj.temp)
    pitchers.proj = clean.rotogrinders(path.pitchers.proj.temp)
    
    hitters.actual = clean.rotoguru(path.players.actual.temp, 
                                    hitters.proj, 
                                    pitchers.proj)[[2]]
    pitchers.actual = clean.rotoguru(path.players.actual.temp, 
                                     hitters.proj, 
                                     pitchers.proj)[[1]]
    
    optimum = create_lineups(15, 4, nonstacked.lineup, 
                             salary.cap, hitters.actual, pitchers.actual)
    print("Created!")
    
    num.teams = c()
    num.points = c()
    
    for(j in 1:nrow(optimum)) {
      lineup = optimum[j,]
      chosen.hitters = lineup[1:nrow(hitters.actual)]
      chosen.pitchers = lineup[(nrow(hitters.actual) + 1):length(lineup)]
      
      hitters.indices = which(chosen.hitters == 1)
      pitchers.indices = which(chosen.pitchers == 1)
      
      teams = c(hitters.actual[hitters.indices, "Team"])
      
      num.teams = append(num.teams, length(unique(teams)))
      
      score = get.optimum(lineup, hitters.actual, pitchers.actual) 
      
      num.points = append(num.points, score)
      
    }
    optimum.teams = list.append(optimum.teams, num.teams)
    optimum.points = list.append(optimum.points, num.points)
  }
  return(list.append(optimum.teams, optimum.points))
}

stacked.lineup = function(hitters, pitchers, lineups, num.overlap,
                          num.hitters, num.pitchers, first.basemen,
                          second.basemen, third.basemen,
                          shortstops, outfielders,
                          catchers, num.teams, hitters.teams,
                          num.games, hitters.games, pitchers.games,
                          salary.cap, pitchers.opponents, consecutive.matrix) {
  w = function(i, j) {
    vapply(seq_along(i), function(k) consecutive.matrix[i[k], j[k]], numeric(1L))
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
    #                  sum_expr(colwise(players.sd[i]) * hitters.lineup[i], i = 1:num.hitters) +
    #                  sum_expr(colwise(players.sd[num.hitters + j]) * pitchers.lineup[j], j = 1:num.pitchers) >= 10) %>%
    
    add_variable(z[i, j], i = 1:num.hitters, j = 1:num.hitters, type = "binary") %>%
    add_constraint(z[i, j] == hitters.lineup[i] + hitters.lineup[j], i= 1:num.hitters, j = 1:num.hitters) %>%
    add_constraint(sum_expr(colwise(w(i,j)) * z[i, j],
                            i = 1:num.hitters, j = 1:num.hitters, i != j) >= 4) %>%
    
    # One of each position besides outfielders
    add_constraint(sum_expr(colwise(first.basemen[i]) * hitters.lineup[i], i = 1:num.hitters) == 1) %>%
    add_constraint(sum_expr(colwise(second.basemen[i]) * hitters.lineup[i], i = 1:num.hitters) == 1) %>%
    add_constraint(sum_expr(colwise(third.basemen[i]) * hitters.lineup[i], i = 1:num.hitters) == 1) %>%
    add_constraint(sum_expr(colwise(shortstops[i]) * hitters.lineup[i], i = 1:num.hitters) == 1) %>%
    add_constraint(sum_expr(colwise(catchers[i]) * hitters.lineup[i], i = 1:num.hitters) == 1) %>%
    add_constraint(sum_expr(colwise(outfielders[i]) * hitters.lineup[i], i = 1:num.hitters) == 3) %>%
    
    
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
                         sum_expr(colwise(players.games[t,i]) * hitters.lineup[t], t = 1:num.hitters) +
                         sum_expr(colwise(players.games[num.hitters + t,i]) * pitchers.lineup[t], t = 1:num.pitchers))
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
  
  result = solve_model(m, with_ROI(solver = "qplk"))
  print(result)
  hitters.df = get_solution(result, hitters.lineup[i])
  pitchers.df = get_solution(result, pitchers.lineup[i])
  return(append(hitters.df[, "value"], pitchers.df[, "value"]))
}

get.scores = function(lineups, hitters, pitchers, 
                      hitters.actual, pitchers.actual) {
  # Points vector
  points = rep(0, nrow(lineups))
  
  for(i in 1:nrow(lineups)) {
    lineup = lineups[i,]
    chosen.hitters = lineup[1:nrow(hitters)]
    chosen.pitchers = lineup[(nrow(hitters) + 1):length(lineup)]
    
    hitters.indices = which(chosen.hitters == 1)
    pitchers.indices = which(chosen.pitchers == 1)
    
    for(j in hitters.indices) {
      name = unlist(strsplit(hitters[j,1], split = " "))
      row = which(grepl(name[1], hitters.actual[,1]) & grepl(name[2], hitters.actual[,1]))
      if(length(row) == 0) {
        name = hitters[j,1]
        for(k in 1:nrow(hitters.actual)) {
          hitter.name = unlist(strsplit(unlist(hitters.actual[k,1]), split = " "))
          if(all(sapply(hitter.name, function(x) {
            grepl(x, name, ignore.case = T) 
          }))) {
            row = k
          }
        }
      }
      if(length(row) == 0) {
        name = unlist(strsplit(hitters[j,1], split = " "))
        team = substring(hitters[j,"Team"], 1, 1)
        row = which(grepl(team, hitters.actual[,"Team"]) & grepl(name[length(name)], hitters.actual[,1]) &
                      grepl(substring(name[length(name)-1], 1, 1), hitters.actual[,1]))
      }
      tryCatch ({
        points[i] = points[i] + hitters.actual[row, "Projection"]
      }, error = function(e) {print(name)})
    }
    
    for(j in pitchers.indices) {
      name = unlist(strsplit(pitchers[j,1], split = " "))
      row = which(grepl(name[1], pitchers.actual[,1]) & grepl(name[2], pitchers.actual[,1]))
      if(length(row) == 0) {
        name = pitchers[j,1]
        for(k in 1:nrow(pitchers.actual)) {
          pitcher.name = unlist(strsplit(unlist(pitchers.actual[k,1]), split = " "))
          if(all(sapply(pitcher.name, function(x) {
            grepl(x, name, ignore.case = T) 
          }))) {
            row = k
          }
        }
      }
      if(length(row) == 0) {
        name = unlist(strsplit(pitchers[j,1], split = " "))
        team = substring(pitchers[j,"Team"], 1, 1)
        row = which(grepl(team, pitchers.actual[,"Team"]) & grepl(name[length(name)], pitchers.actual[,1]) &
                      grepl(substring(name[length(name)-1], 1, 1), pitchers.actual[,1]))
      }
      tryCatch ({
        points[i] = points[i] + pitchers.actual[row, "Projection"]
      }, error = function(e) {print(name)})
    }
  }
  
  return(points)
}


## Returns the optimal lineup performance 

## ------------------------------------------------------------ ##


get.optimum = function(df, hitters.actual, pitchers.actual) {
  lineup = df
  chosen.hitters = lineup[1:nrow(hitters.actual)]
  chosen.pitchers = lineup[(nrow(hitters.actual) + 1):length(lineup)]
  
  hitters.indices = which(chosen.hitters == 1)
  pitchers.indices = which(chosen.pitchers == 1)
  
  points = sum(hitters.actual[hitters.indices, "Projection"]) +
    sum(pitchers.actual[pitchers.indices, "Projection"])
  
  return(points)
}


## Cleans Rotoguru CSV files for backtesting

## ------------------------------------------------------------ ##


clean.rotoguru = function(path.roto, 
                          hitters.proj, 
                          pitchers.proj) {
  df = read.csv(path.roto, 
                stringsAsFactors = F)[1:5]
  names(df) = c("Name",
                "Projection",
                "Salary",
                "Team",
                "Opponent")
  df$Name = gsub("\\^", "", df$Name)
  df$Name = gsub("[0-9]+", "", df$Name)
  
  temp.pitchers = lapply(df$Name, function(x) {
    unlist(strsplit(x, split = " "))
  })
  
  temp.pitchers = lapply(temp.pitchers, function(x) {
    x[x != ""]
  })
  
  df$Name = lapply(temp.pitchers, function(x) {
    paste(x, collapse = " ")
  })
  
  df$Salary        = gsub(",", "", df$Salary)
  df$Salary        = suppressWarnings(as.numeric(gsub("\\$", "", df$Salary))) 
  df$Team          = gsub(" ", "", df$Team)
  df$Opponent      = gsub("v ", "", df$Opponent)
  df$Opponent      = gsub("@ ", "", df$Opponent)
  df$Opponent      = toupper(df$Opponent)
  df$Opponent      = gsub(" ", "", df$Opponent)
  df$Teams.Playing = paste(df$Team, df$Opponent, sep = "@")
  df$Position      = NA
  split            = which(df[,1] == "Hitters")
  pitchers.df      = df[1:(split-1),]
  hitters.df       = df[(split+1):nrow(df),]
  
  for(i in 1:nrow(pitchers.df)) {
    name = unlist(strsplit(unlist(pitchers.df[i,1]), split = " "))
    row = which(grepl(name[1], pitchers.proj[,1]) & grepl(name[2], pitchers.proj[,1]))
    if(length(row) == 0) {
      name = unlist(pitchers.df[i,1])
      for(j in 1:nrow(pitchers.proj)) {
        pitcher.name = unlist(strsplit(pitchers.proj[j,1], split = " "))
        if(all(sapply(pitcher.name, function(x) {
          grepl(x, name, ignore.case = T) 
        }))) {
          row = j
        }
      }
    }
    if(length(row) == 0) {
      name = unlist(strsplit(unlist(pitchers.df[i,1]), split = " "))
      team = substring(pitchers.df[i,"Team"], 1, 1)
      row  = which(grepl(team, pitchers.proj[,"Team"]) & 
                     grepl(name[2], pitchers.proj[,1]) &
                     grepl(substring(name[1], 1, 1), pitchers.proj[,1]))
    }
    tryCatch ({
      pitchers.df[i,"Salary"]   = pitchers.proj[row, "Salary"]
      pitchers.df[i,"Position"] = pitchers.proj[row, "Position"]
    }, error = function(e) {})
  }
  
  for(i in 1:nrow(hitters.df)) {
    name = unlist(strsplit(unlist(hitters.df[i,1]), split = " "))
    row  = which(grepl(name[1], hitters.proj[,1]) & grepl(name[2], hitters.proj[,1]))
    if(length(row) == 0) {
      name = unlist(hitters.df[i,1])
      for(j in 1:nrow(hitters.proj)) {
        hitter.name = unlist(strsplit(hitters.proj[j,1], split = " "))
        if(all(sapply(hitter.name, function(x) {
          grepl(x, name, ignore.case = T) 
        }))) {
          row = j
        }
      }
    }
    if(length(row) == 0) {
      name = unlist(strsplit(unlist(hitters.df[i,1]), split = " "))
      team = substring(hitters.df[i,"Team"], 1, 1)
      row  = which(grepl(team, hitters.proj[,"Team"]) & 
                     grepl(name[2], hitters.proj[,1]) &
                     grepl(substring(name[1], 1, 1), hitters.proj[,1]))
    }
    tryCatch ({
      hitters.df[i,"Salary"] = hitters.proj[row, "Salary"]
      hitters.df[i,"Position"] = hitters.proj[row, "Position"]
    }, error = function(e) {})
  }
  pitchers.df            = na.omit(pitchers.df)
  hitters.df             = na.omit(hitters.df)
  pitchers.df$Projection = as.numeric(gsub(" ", "", pitchers.df$Projection))
  hitters.df$Projection  = as.numeric(gsub(" ", "", hitters.df$Projection))
  
  return(list(pitchers = pitchers.df, 
              hitters = hitters.df))
}

