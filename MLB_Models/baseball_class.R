## Required libraries

## ------------------------------------------------------------ ##


library(magrittr)
library(rlist)
library(gurobi)
library(Matrix)
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
library(mice)
library(mgcv)
library(nlme)
library(mvtboost)

## Paths to folders containing scraped data

## ------------------------------------------------------------ ##


path.roto.hitters = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Roto_Hitters"
path.roto.pitchers = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Roto_Pitchers"
path.saber = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Saber_Sim"
path.swish = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Swish_Analytics"
path.nerd = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Fantasy_Nerd"
output = "C:/Users/Ming/Documents/Fantasy_Models/output"
path.output = "C:/Users/Ming/Documents/Fantasy_Models/output/MLB_consecutive.csv"


## Cleans Rotogrinders CSV files

## ------------------------------------------------------------ ##


hitter.names = function(path.saber.file) {
  saber.df = read.csv(path.saber.file,
                      stringsAsFactors = F)
  saber.df = saber.df[saber.df$Position != "P",]
  saber.df = subset(saber.df,
                    select = c(Name, Actual, Projection, dk_95_percentile))
  saber.df$Sigma = saber.df$dk_95_percentile - saber.df$Projection
  return(subset(saber.df, select = c(Name, Actual, Projection, Sigma)))
}

pitcher.names = function(path.saber.file) {
  saber.df = read.csv(path.saber.file,
                      stringsAsFactors = F)
  saber.df = saber.df[saber.df$Position == "P",]
  saber.df = subset(saber.df,
                    select = c(Name, Actual, Projection, dk_95_percentile))
  saber.df$Sigma = saber.df$dk_95_percentile - saber.df$Projection
  return(subset(saber.df, select = c(Name, Actual, Projection, Sigma)))
}

clean.rotogrinders = function(roto.path, is.hitter) {
  df = read.csv(roto.path,
                stringsAsFactors = F)
  player.df = subset(df, select = c(Name,
                             Salary,
                             Team,
                             Position,
                             Opp))
  names(player.df) = c("Name",
                       "Salary",
                       "Team",
                       "Position",
                       "Opponent")
  if(is.hitter) {
    player.df = subset(df, select = c(Name,
                                      Salary,
                                      Team,
                                      Position,
                                      Opp,
                                      Order))
    names(player.df) = c("Name",
                         "Salary",
                         "Team",
                         "Position",
                         "Opponent",
                         "Order")
    df$Order = as.numeric(df$Order)
  }
  df = player.df
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
  roto.file = clean.rotogrinders(roto.path, is.hitter)
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
        merged.file[i,5:ncol(merged.file)] = roto.file[row,2:ncol(roto.file)]
      }, error = function(e) {})
    }
  }
  if(is.hitter) {
    merged.file$Order = as.numeric(merged.file$Order)
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


## Type A Stacking: Create one lineup using Gurobi LP/QP

## ------------------------------------------------------------ ##

stacked.lineup.a = function(hitters, pitchers, lineups, num.overlap,
                            num.hitters, num.pitchers, first.basemen,
                            second.basemen, third.basemen,
                            shortstops, catchers,
                            outfielders, num.teams, hitters.teams,
                            num.games, hitters.games, players.games,
                            salary.cap, pitchers.opponents, consecutive.matrix,
                            abs.weight, rel.weight, multi) {
  
  model = list()
  
  # Dimensions of constraint matrix
  nCol = num.hitters + num.pitchers + num.teams + num.games
  
  # Creating empty binary slots for model variables
  model$vtype = rep("B", num.hitters + num.pitchers + num.teams + num.games)
  
  # Model objective
  model$modelsense = "max"
  
  # List that stores all constraints
  constraints = list()
  
  # Eight hitters constraint
  hitters.constraint = append(rep(1, num.hitters),
                              rep(0, num.pitchers + num.teams + num.games))
  constraints = list.append(constraints, hitters.constraint)
  
  model$sense = c("=")
  model$rhs = c(8)
  
  # Two pitchers constraint
  pitchers.constraint = rep(0, nCol) 
  pitchers.constraint[(num.hitters+1):(num.hitters+num.pitchers)] = 1
  constraints = list.append(constraints, pitchers.constraint)
  
  model$sense = append(model$sense, c("="))
  model$rhs = append(model$rhs, c(2))
  
  # Hitter type constraints
  firstbase.constraint = append(first.basemen, 
                                rep(0, num.pitchers + num.teams + num.games))
  secondbase.constraint = append(second.basemen,
                                 rep(0, num.pitchers + num.teams + num.games))
  thirdbase.constraint = append(third.basemen,
                                rep(0, num.pitchers + num.teams + num.games))
  shortstops.constraint = append(shortstops,
                                 rep(0, num.pitchers + num.teams + num.games))
  catchers.constraint = append(catchers,
                               rep(0, num.pitchers + num.teams + num.games))
  outfielders.constraint = append(outfielders,
                                  rep(0, num.pitchers + num.teams + num.games))
  
  type.constraints = list(firstbase.constraint,
                          secondbase.constraint,
                          thirdbase.constraint,
                          shortstops.constraint,
                          catchers.constraint,
                          outfielders.constraint)
  
  constraints = c(constraints, type.constraints)
  
  model$sense = append(model$sense, rep("=", 6))
  model$rhs = append(model$rhs, c(1, 1, 1, 1, 1, 3))
  
  # Budget constraint
  salary.constraint = rep(0, nCol)
  salary.constraint[1:(num.hitters+num.pitchers)] = append(hitters[,"Salary"], pitchers[,"Salary"])
  constraints = list.append(constraints, salary.constraint)
  
  model$sense = append(model$sense, c("<"))
  model$rhs = append(model$rhs, c(salary.cap))
  
  # At least two teams, and no more than five hitters from the same team
  team.constraints.a = list()
  team.constraints.b = list()
  getIdx = function(idx, len) {
    vect = rep(0, len)
    vect[idx] = 1
    return(vect)
  }
  for(i in 1:num.teams) {
    lfs.a = rep(0, nCol)
    lfs.a[1:num.hitters] = hitters.teams[,i]
    lfs.a[(num.hitters+num.pitchers+1):(num.hitters+num.pitchers+num.teams)] = -1 * getIdx(i, num.teams)
    lfs.b = lfs.a
    lfs.b[(num.hitters+num.pitchers+1):(num.hitters+num.pitchers+num.teams)] = -5 * getIdx(i, num.teams)
    
    constraints = list.append(constraints, lfs.a)
    constraints = list.append(constraints, lfs.b)
    
    model$sense = append(model$sense, c(">", "<"))
    model$rhs = append(model$rhs, c(0, 0))
  }
  team.size = rep(0, nCol)
  team.size[(num.hitters+num.pitchers+1):(num.hitters+num.pitchers+num.teams)] = rep(1, num.teams)
  
  constraints = list.append(constraints, team.size)
  
  model$sense = append(model$sense, c(">"))
  model$rhs = append(model$rhs, c(2))
  
  # Players must come from at least two different games
  for(k in 1:num.games) {
    lfs = append(players.games[,k], rep(0, num.teams))
    lfs = append(lfs,  -1 * getIdx(k, num.games))
    
    constraints = list.append(constraints, lfs)
    
    model$sense = append(model$sense, c(">"))
    model$rhs = append(model$rhs, c(0))
  }
  
  game.number = rep(0, nCol)
  game.number[(num.hitters+num.pitchers+num.teams+1):nCol] = rep(1, num.games)
  
  constraints = list.append(constraints, game.number)
  
  model$sense = append(model$sense, c(">"))
  model$rhs = append(model$rhs, c(2))
  
  # Pitcher stacking: no hitter and pitcher from opposing teams
  for(i in 1:num.pitchers) {
    lfs = append(pitchers.opponents[i,], 5 * getIdx(i, num.pitchers))
    lfs = append(lfs, rep(0, num.teams + num.games))
    
    constraints = list.append(constraints, lfs)
    
    model$sense = append(model$sense, c("<"))
    model$rhs = append(model$rhs, c(5))
  }
  
  # Overlap constraint: maximum number of shared players between any two lineups
  for(i in 1:nrow(lineups)) {
    overlap.constraints = append(lineups[i,],
                                 rep(0, num.teams + num.games))
    
    constraints = list.append(constraints, overlap.constraints)
    
    model$sense = append(model$sense, c("<"))
    model$rhs = append(model$rhs, c(num.overlap))
  }
  
  # Consecutive hitter stacking
  empty.matrix = matrix(rep(0, nCol * nCol), ncol = nCol)
  empty.matrix[1:num.hitters, 1:num.hitters] = consecutive.matrix
  sparse.consecutive.matrix = Matrix(empty.matrix, sparse = T)

  qc1 = list()
  qc1$Qc = sparse.consecutive.matrix
  qc1$sense = ">"
  qc1$rhs = 16

  model$quadcon = list(qc1)
  
  # No pitcher can be chosen more than 50 times
  # for(i in 1:num.pitchers) {
  #   lhs = rep(0, nCol)
  #   lhs[num.hitters + i] = 1
  #   constraints = list.append(constraints, lhs)
  #   model$sense = append(model$sense, c("<"))
  #   model$rhs = append(model$rhs, c(50 - sum(lineups[,(num.hitters + i)])))
  # }
  
  # Convert linear constraints into a sparse matrix
  constraints = Reduce(function(x, y) {
    rbind(x, y)
  }, constraints)
  
  model$A = Matrix(constraints, sparse = T)
  
  obj.a = append(hitters[,"Projection"], pitchers[,"Projection"])
  obj.a = append(obj.a, rep(0, num.teams + num.games))
  
  obj.b = append(hitters[,"Sigma"], pitchers[,"Sigma"])
  obj.b = append(obj.b, rep(0, num.teams + num.games))
  
  Set = list(obj.a, obj.b)
  SetObjPriority = c(2,1)
  SetObjWeight = c(1.0, abs.weight)
  
  if(!multi) {
    # Model objective
    model$obj = obj.a
  } else {
    model$multiobj  = list()
    for (m in 1:2) {
      model$multiobj[[m]] = list()
      model$multiobj[[m]]$objn = Set[[m]]
      model$multiobj[[m]]$priority = SetObjPriority[m]
      model$multiobj[[m]]$weight   = SetObjWeight[m]
      model$multiobj[[m]]$abstol   = m
      model$multiobj[[m]]$reltol   = rel.weight
      model$multiobj[[m]]$con      = 0.0
    }
  }
  params = list()
  params$LogToConsole = 0
  result = gurobi(model, params)
  # print(result$objval)
  return(result$x[1:(num.hitters + num.pitchers)])
}



## Create desired number of lineups with desired model

## ------------------------------------------------------------ ##


is.consecutive = function(hitters, i, j) {
  if(hitters[i, "Team"] != hitters[j, "Team"]) {
    return(FALSE)
  }
  else if(abs(hitters[i, "Order"] - hitters[j, "Order"]) <= 2) {
    return(TRUE)
  }
  else if(abs((max(hitters[i, "Order"], hitters[j, "Order"]) - 9) -
          min(hitters[i, "Order"], hitters[j, "Order"])) <= 2) {
    return(TRUE)
  }
  return(FALSE)
}

create_lineups = function(num.lineups, num.overlap, formulation, salary.cap,
                          hitters, pitchers, abs.weight, rel.weight) {
  
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
    if(grepl("C", hitters[i, "Position"])) {
      hitters.matrix[5, i] = 1
    }
    if(grepl("OF", hitters[i, "Position"])) {
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
  players = rbind(subset(hitters, select = -c(Order)), pitchers)
  games = unique(players[,"Teams.Playing"])
  
  duplicated = c()
  for(game in games) {
    game = unlist(strsplit(game, split = "@"))
    if(length(which(grepl(game[1], duplicated) & 
             grepl(game[2], duplicated))) > 0) {
      games = games[games != paste(game[1], game[2], sep = "@")]
    }
    else {
      duplicated = append(duplicated, paste(game[1], game[2], sep = "@"))
    }
  }
  
  num.games = length(games)
  games.distribution = list()
  
  for(i in 1:nrow(players)) {
    player.info = rep(0, num.games) 
    team = unlist(strsplit(players[i, "Teams.Playing"], split = "@"))
    for(j in 1:num.games) {
      if(grepl(team[1], games[j]) & 
         grepl(team[2], games[j])) {
        player.info[j] = 1
      }
    }
    games.distribution = list.append(games.distribution, player.info)
  }
  
  # Converting list into a matrix
  players.games = Reduce(function(x, y) {
    rbind(x, y)
  }, games.distribution)
  
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
  
  empty = rep(0, num.hitters * num.hitters)
  consecutive.matrix = matrix(empty, nrow = num.hitters)

  for(i in 1:num.hitters) {
    for(j in 1:num.hitters) {
      consecutive.matrix[i, j] = ifelse(is.consecutive(hitters, i, j), 1, 0)
    }
  }
  
  
  # Create a lineup
  tracer = matrix(rep(0, num.hitters + num.pitchers), nrow = 1)
  lineups = formulation(hitters, pitchers, tracer, num.overlap,
                        num.hitters, num.pitchers, hitters.list[[1]],
                        hitters.list[[2]], hitters.list[[3]],
                        hitters.list[[4]], hitters.list[[5]],
                        hitters.list[[6]], num.teams, hitters.teams,
                        num.games, hitters.games, players.games,
                        salary.cap, pitchers.opponents, consecutive.matrix,
                        abs.weight, rel.weight, TRUE)
  lineups = matrix(lineups, nrow = 1)
  
  if(num.lineups > 1) {
    for(i in 1:(num.lineups - 1)) {
      lineup = formulation(hitters, pitchers, lineups, num.overlap,
                           num.hitters, num.pitchers, hitters.list[[1]],
                           hitters.list[[2]], hitters.list[[3]],
                           hitters.list[[4]], hitters.list[[5]],
                           hitters.list[[6]], num.teams, hitters.teams,
                           num.games, hitters.games, players.games,
                           salary.cap, pitchers.opponents, consecutive.matrix,
                           abs.weight, rel.weight, TRUE)
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
    
    hitters.indices = which(chosen.hitters > 0.5)
    pitchers.indices = which(chosen.pitchers > 0.5)
    
    names = append(c(hitters[hitters.indices, "Name"]),
                   c(pitchers[pitchers.indices, "Name"]))
    
    names = append(names,
                   c(hitters[hitters.indices, "Position"]))
    
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


get.scores = function(lineups, hitters, pitchers) {
  # Points vector
  points = rep(0, nrow(lineups))
  
  for(i in 1:nrow(lineups)) {
    lineup = lineups[i,]
    chosen.hitters = lineup[1:nrow(hitters)]
    chosen.pitchers = lineup[(nrow(hitters) + 1):length(lineup)]
    
    hitters.indices = which(chosen.hitters > 0.5)
    pitchers.indices = which(chosen.pitchers > 0.5)
    
    points[i] = sum(hitters[hitters.indices, "Actual"]) +
      sum(pitchers[pitchers.indices, "Actual"])
  }
  
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
  max.scores = list()
  saber.files = list.files(path.saber)[c(200, 220, 280, 320, 340, 380)]
  dates = lapply(saber.files, function(x) {
    unlist(strsplit(gsub("[A-z\\.]", "", x), split = " "))[1]
  })
  for(overlap in overlaps) {
    for(d in 1:length(dates)) {
      tryCatch({
        date = dates[[d]]
        path.hitters.proj.temp = gsub.custom(path.hitters.proj, date)
        path.pitchers.proj.temp = gsub.custom(path.pitchers.proj, date)
        
        saber.file = paste(path.saber, saber.files[[d]], sep = "/")
        
        hitters.proj = merge.rotogrinders(path.hitters.proj.temp, saber.file, TRUE)
        pitchers.proj = merge.rotogrinders(path.pitchers.proj.temp, saber.file, FALSE)
        
        weights.a = c(0.05, 0.1, 0.2, 0.5)
        weights.b = c(0, 0.01, 0.02, 0.05, 0.1)
        
        for(first.weight in weights.a) {
          for(second.weight in weights.b) {
            df = create_lineups(num.lineups, overlap, 
                                stacked.lineup.a, salary.cap, 
                                hitters.proj, pitchers.proj,
                                first.weight, second.weight)
            scores = get.scores(df, hitters.proj, pitchers.proj)
            max.scores = list.append(max.scores, name = max(scores))
            names(max.scores)[which(names(max.scores) == "name")] = 
              paste(toString(first.weight), toString(second.weight), sep = "//")
          }
        }
        
        
        # file_name = paste("modelb", toString(overlap), sep = "")
        # setwd(output)
        # write(max(scores), file = paste(file_name, ".txt", sep = ""), append = T)
        
      }, error = function(e) {})
    }
  }
  return(max.scores)
}
