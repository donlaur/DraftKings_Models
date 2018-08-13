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
library(parallel)
library(doParallel)
library(caret)
library(reshape2)
library(corrplot)
library(leaps)
library(caretEnsemble)


## Paths to folders containing scraped data

## ------------------------------------------------------------ ##


path.roto.hitters = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Hitters"
path.roto.pitchers = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Pitchers"
path.saber = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Saber_Sim"
path.swish = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Swish_Analytics"
path.nerd = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Fantasy_Nerd"


## Cleans Rotogrinders CSV files

## ------------------------------------------------------------ ##


hitter.names = function(path.saber.file) {
  saber.df = read.csv(path.saber.file,
                      stringsAsFactors = F)
  saber.df = saber.df[saber.df$Position != "P",]
  saber.df = as.data.frame(saber.df[,"Name"])
  names(saber.df) = "Name"
  return(saber.df)
}

pitcher.names = function(path.saber.file) {
  saber.df = read.csv(path.saber.file,
                      stringsAsFactors = F)
  saber.df = saber.df[saber.df$Position == "P",]
  saber.df = as.data.frame(saber.df[,"Name"])
  names(saber.df) = "Name"
  return(saber.df)
}

clean.rotogrinders = function(roto.path) {
  df = read.csv(roto.path,
                stringsAsFactors = F)
  df = subset(df, select = c(Name,
                             Salary,
                             Team,
                             Position,
                             Opp,
                             Points))
  names(df) = c("Name",
                "Salary",
                "Team",
                "Position",
                "Opponent",
                "Projection")
  df$Salary = gsub(c("K"), "", df$Salary)
  df$Salary = gsub(c("\\$"), "", df$Salary)
  df$Salary = as.numeric(df$Salary) * 1000
  df$Team = gsub("@", "", df$Team)
  df$Team = gsub(" ", "", df$Team)
  df$Opponent = gsub("@", "", df$Opponent)
  df$Opponent = gsub(" ", "", df$Opponent)
  df$Teams.Playing = paste(df$Team, df$Opponent, sep = "@")
  df$Teams.Playing = gsub(" ", "", df$Teams.Playing)
  return(df)
}

merge.rotogrinders = function(roto.path, path.saber.file, is.hitter) {
  roto.file = clean.rotogrinders(roto.path)
  saber.file = hitter.names(path.saber.file)
  if(!is.hitter) {
    saber.file = pitcher.names(path.saber.file)
  }
  merged.file = merge(saber.file,
                      roto.file,
                      by = "Name",
                      all.x = T)
  merged.file$Name = as.character(merged.file$Name)
  for(i in 1:nrow(merged.file)) {
    if(anyNA(merged.file[i,])) {
      row = 0
      name = merged.file[i,1]
      for(j in 1:nrow(roto.file)) {
        player.name = unlist(strsplit(roto.file[j,1], split = " "))
        if(all(sapply(player.name, function(x) {
          grepl(x, name, ignore.case = T) 
        }))) {
          row = j
        }
      }
      if(row == 0) {
        name = unlist(strsplit(merged.file[i,1], split = " "))
        row = which(grepl(name[length(name)], roto.file[,1]) &
                      grepl(substring(name[length(name) - 1], 1, 1), roto.file[,1]))
      }
      tryCatch ({
        merged.file[i,] = roto.file[row,]
      }, error = function(e) {})
    }
  }
  return(merged.file[complete.cases(merged.file),])
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
  
  df$Salary = gsub(",", "", df$Salary)
  df$Salary = suppressWarnings(as.numeric(gsub("\\$", "", df$Salary))) 
  df$Team = gsub(" ", "", df$Team)
  df$Opponent = gsub("v ", "", df$Opponent)
  df$Opponent = gsub("@ ", "", df$Opponent)
  df$Opponent = toupper(df$Opponent)
  df$Opponent = gsub(" ", "", df$Opponent)
  df$Teams.Playing = paste(df$Team, df$Opponent, sep = "@")
  df$Position = NA
  split = which(df[,1] == "Hitters")
  pitchers.df = df[1:(split-1),]
  hitters.df = df[(split+1):nrow(df),]
  
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
      row = which(grepl(team, pitchers.proj[,"Team"]) & 
                    grepl(name[2], pitchers.proj[,1]) &
                    grepl(substring(name[1], 1, 1), pitchers.proj[,1]))
    }
    tryCatch ({
      pitchers.df[i,"Salary"] = pitchers.proj[row, "Salary"]
      pitchers.df[i,"Position"] = pitchers.proj[row, "Position"]
    }, error = function(e) {})
  }
  
  for(i in 1:nrow(hitters.df)) {
    name = unlist(strsplit(unlist(hitters.df[i,1]), split = " "))
    row = which(grepl(name[1], hitters.proj[,1]) & grepl(name[2], hitters.proj[,1]))
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
      row = which(grepl(team, hitters.proj[,"Team"]) & 
                    grepl(name[2], hitters.proj[,1]) &
                    grepl(substring(name[1], 1, 1), hitters.proj[,1]))
    }
    tryCatch ({
      hitters.df[i,"Salary"] = hitters.proj[row, "Salary"]
      hitters.df[i,"Position"] = hitters.proj[row, "Position"]
      }, error = function(e) {})
  }
  pitchers.df = na.omit(pitchers.df)
  hitters.df = na.omit(hitters.df)
  pitchers.df$Projection = as.numeric(gsub(" ", "", pitchers.df$Projection))
  hitters.df$Projection = as.numeric(gsub(" ", "", hitters.df$Projection))
  return(list(pitchers = pitchers.df, 
              hitters = hitters.df))
}


## Create one stacked lineup using integer linear programming

## ------------------------------------------------------------ ##


stacked.lineup = function(hitters, pitchers, lineups, num.overlap,
                          num.hitters, num.pitchers, first.basemen,
                          second.basemen, third.basemen,
                          shortstops, outfielders,
                          catchers, num.teams, hitters.teams,
                          num.games, hitters.games, pitchers.games,
                          salary.cap, pitchers.opponents) {
  # w = function(i, j) {
  #   vapply(seq_along(i), function(k) hitters.covariance[i[k], j[k]], numeric(1L))
  # }
  # 
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
  m = add_constraint(m, sum_expr(used.team[i], i = 1:num.teams) <= 3)
  
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
  
  result = solve_model(m, with_ROI(solver = "glpk"))
  hitters.df = get_solution(result, hitters.lineup[i])
  pitchers.df = get_solution(result, pitchers.lineup[i])
  return(append(hitters.df[, "value"], pitchers.df[, "value"]))
}



## Create one nonstacked lineup using integer linear programming

## ------------------------------------------------------------ ##


nonstacked.lineup = function(hitters, pitchers, lineups, num.overlap,
                             num.hitters, num.pitchers, first.basemen,
                             second.basemen, third.basemen,
                             shortstops, outfielders,
                             catchers, num.teams, hitters.teams,
                             num.games, hitters.games, pitchers.games,
                             salary.cap, pitchers.opponents) {
  # w = function(i, j) {
  #   vapply(seq_along(i), function(k) hitters.covariance[i[k], j[k]], numeric(1L))
  # }
  # 
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
                         sum_expr(colwise(hitters.games[t,i]) * hitters.lineup[t], t = 1:num.hitters) +
                         sum_expr(colwise(pitchers.games[t,i]) * pitchers.lineup[t], t = 1:num.pitchers))
  }
  m = add_constraint(m, sum_expr(used.game[i], i = 1:num.games) >= 2)
  
  # Overlap constraint
  for(i in 1:nrow(lineups)) {
    m = add_constraint(m, sum_expr(colwise(lineups[i,j]) * hitters.lineup[j], j = 1:num.hitters) +
                         sum_expr(colwise(lineups[i, num.hitters + j]) * pitchers.lineup[j], j = 1:num.pitchers) <= num.overlap)
  }
  
  m = set_objective(m,
                    sum_expr(colwise(hitters[i, "Projection"]) * hitters.lineup[i], i = 1:num.hitters) +
                      sum_expr(colwise(pitchers[i, "Projection"]) * pitchers.lineup[i], i = 1:num.pitchers),
                    sense = "max")
  
  result = solve_model(m, with_ROI(solver = "glpk"))
  hitters.df = get_solution(result, hitters.lineup[i])
  pitchers.df = get_solution(result, pitchers.lineup[i])
  return(append(hitters.df[, "value"], pitchers.df[, "value"]))
}


## Create desired number of lineups with desired model

## ------------------------------------------------------------ ##


# is.consecutive = function(i, j) {
#   if(hitters[i, "Team"] != hitters[j, "Team"] | hitters[i, "Order"] == hitters[j, "Order"]) {
#     return(FALSE)
#   }
#   else if(abs(hitters[i, "Order"] - hitters[j, "Order"]) <= 2) {
#     return(TRUE)
#   }
#   else if(abs((max(hitters[i, "Order"], hitters[j, "Order"]) - 9) -
#           min(hitters[i, "Order"], hitters[j, "Order"]))) {
#     return(TRUE)
#   }
#   return(FALSE)
# }

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
  
  # Mock variance vector
  # players.sd = append(hitters$Sigma, pitchers$Sigma)
  
  # Covariance matrix
  # hitters.covariance = get.cov(hitter.list, hitters)
  
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
  
  # n by n matrix, where each entry (x,y) is 1 if the hitter in row x is within the same five-person
  # lineup as the hitter in row y, and 0 otherwise
  
  # empty = rep(0, num.hitters * num.hitters)
  # consecutive.matrix = matrix(empty, nrow = num.hitters)
  # 
  # for(i in 1:num.hitters) {
  #   for(j in 1:num.hitters) {
  #     consecutive.matrix[i][j] = ifelse((hitters[i, "Team"] == hitters[j, "Team"] &
  #                                         abs(hitters[i, "Order"] - hitters[j, "Order"]) == 1),
  #                                       1, 0)
  #   }
  # }
  
  
  # Create a lineup
  tracer = matrix(rep(0, num.hitters + num.pitchers), nrow = 1)
  lineups = formulation(hitters, pitchers, tracer, num.overlap,
                        num.hitters, num.pitchers, hitters.list[[1]],
                        hitters.list[[2]], hitters.list[[3]],
                        hitters.list[[4]], hitters.list[[5]],
                        hitters.list[[6]], num.teams, hitters.teams,
                        num.games, hitters.games, pitchers.games,
                        salary.cap, pitchers.opponents)
  lineups = matrix(lineups, nrow = 1)
  
  if(num.lineups > 1) {
    for(i in 1:(num.lineups - 1)) {
      lineup = formulation(hitters, pitchers, lineups, num.overlap,
                           num.hitters, num.pitchers, hitters.list[[1]],
                           hitters.list[[2]], hitters.list[[3]],
                           hitters.list[[4]], hitters.list[[5]],
                           hitters.list[[6]], num.teams, hitters.teams,
                           num.games, hitters.games, pitchers.games,
                           salary.cap, pitchers.opponents)
      lineups = rbind(lineups, lineup)
    }
  }
  
  return(lineups)
}


## Takes the lineups matrix and writes it to a CSV file, where each
## row is a lineup

## ------------------------------------------------------------ ##


lineups.to.csv = function(lineups, hitters, pitchers, path.output) {
  for(i in 1:nrow(lineups)) {
    lineup = lineups[i,]
    chosen.hitters = lineup[1:nrow(hitters)]
    chosen.pitchers = lineup[(nrow(hitters) + 1):length(lineup)]
    
    hitters.indices = which(chosen.hitters == 1)
    pitchers.indices = which(chosen.pitchers == 1)
    
    names = append(c(hitters[hitters.indices, "Name"]),
                   c(pitchers[pitchers.indices, "Name"]))
    
    points = sum(hitters[hitters.indices, "Projection"]) +
      sum(pitchers[pitchers.indices, "Projection"])
    
    names = append(names, toString(points))
    
    write.table(matrix(names, nrow = 1),
                path.output,
                sep = ",",
                append = T,
                col.names = F,
                row.names = F)
  }
  print(paste("CSV successfully exported to ", path.output, sep = " "))
}


## Takes the lineups matrix and returns a vector of scores, where
## each score is the score of the respective lineup

## ------------------------------------------------------------ ##


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


## Backtesting on historical Draftkings data

## ------------------------------------------------------------ ##


gsub.custom = function(str, date) {
  str = gsub("DATE", date, str)
  return(str)
} 

backtest = function(overlaps, salary.cap,
                    num.lineups, path.hitters.proj,
                    path.pitchers.proj, path.players.actual,
                    path.saber) {
  setwd(path.saber)
  saber.files = list.files()[seq(200, 400, by = 20)]
  dates = lapply(saber.files, function(x) {
    unlist(strsplit(gsub("[A-z\\.]", "", x), split = " "))[1]
  })
  for(overlap in overlaps) {
    for(i in 1:length(dates)) {
      date = dates[[i]]
      path.hitters.proj.temp = gsub.custom(path.hitters.proj, date)
      path.pitchers.proj.temp = gsub.custom(path.pitchers.proj, date)
      path.players.actual.temp = gsub.custom(path.players.actual, date)
      
      saber.file = paste(path.saber, saber.files[[i]], sep = "/")
      
      hitters.proj = merge.rotogrinders(path.hitters.proj.temp, saber.file, TRUE)
      pitchers.proj = merge.rotogrinders(path.pitchers.proj.temp, saber.file, FALSE)
      
      hitters.actual = clean.rotoguru(path.players.actual.temp, 
                                      hitters.proj, 
                                      pitchers.proj)[[2]]
      pitchers.actual = clean.rotoguru(path.players.actual.temp, 
                                       hitters.proj, 
                                       pitchers.proj)[[1]]
      
      df = create_lineups(num.lineups, overlap, 
                          stacked.lineup, salary.cap, 
                          hitters.proj, pitchers.proj)
      print("Created!")
      scores = get.scores(df, hitters.proj, pitchers.proj, 
                          hitters.actual, pitchers.actual) 
      file_name = paste("modelb", toString(overlap), sep = "")
      write(max(scores), file = paste(file_name, ".txt", sep = ""), append = T)
    }
  }
}
