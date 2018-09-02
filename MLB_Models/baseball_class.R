## Extends baseball_dataset and baseball_objectives. 
## Provides functions for generating lineups and backtesting 
## lineup performance against real historical data.


source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_dataset.R")
source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_objectives.R")


## Set up a model with constraints using Gurobi

## ------------------------------------------------------------ ##


setup.model = function(hitters, pitchers, lineups, num.overlap,
                       num.hitters, num.pitchers, first.basemen,
                       second.basemen, third.basemen,
                       shortstops, catchers,
                       outfielders, num.teams, hitters.teams,
                       num.games, hitters.games, players.games,
                       salary.cap, pitchers.opponents, consecutive.matrix) {
  
  model = list()
  
  # Dimensions of constraint matrix
  nCol             = num.hitters + num.pitchers + num.teams + num.games
  
  # Creating empty binary slots for model variables
  model$vtype      = rep("B", num.hitters + num.pitchers + num.teams + num.games)
  
  # Model objective
  model$modelsense = "max"
  
  # List that stores all constraints
  constraints      = list()
  
  # Eight hitters constraint
  hitters.constraint = append(rep(1, num.hitters),
                              rep(0, num.pitchers + num.teams + num.games))
  constraints        = list.append(constraints, hitters.constraint)
  
  model$sense        = c("=")
  model$rhs          = c(8)
  
  # Two pitchers constraint
  p.start                            = num.hitters + 1
  p.end                              = num.hitters + num.pitchers
  pitchers.constraint                = rep(0, nCol) 
  pitchers.constraint[p.start:p.end] = 1
  constraints                        = list.append(constraints, 
                                                   pitchers.constraint)
  
  model$sense = append(model$sense, c("="))
  model$rhs   = append(model$rhs, c(2))
  
  # Hitter type constraints
  firstbase.constraint   = append(first.basemen, 
                                  rep(0, num.pitchers + num.teams + num.games))
  secondbase.constraint  = append(second.basemen,
                                  rep(0, num.pitchers + num.teams + num.games))
  thirdbase.constraint   = append(third.basemen,
                                  rep(0, num.pitchers + num.teams + num.games))
  shortstops.constraint  = append(shortstops,
                                  rep(0, num.pitchers + num.teams + num.games))
  catchers.constraint    = append(catchers,
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
  model$rhs   = append(model$rhs, c(1, 1, 1, 1, 1, 3))
  
  # Budget constraint
  salary.constraint          = rep(0, nCol)
  salary.constraint[1:p.end] = append(hitters[,"Salary"], pitchers[,"Salary"])
  constraints                = list.append(constraints, salary.constraint)
  
  model$sense = append(model$sense, c("<"))
  model$rhs   = append(model$rhs, c(salary.cap))
  
  # At least two teams, and no more than five hitters from the same team
  team.constraints.a = list()
  team.constraints.b = list()
  t.start            = num.hitters + num.pitchers + 1
  t.end              = num.hitters + num.pitchers + num.teams
  getIdx = function(idx, len) {
    vect      = rep(0, len)
    vect[idx] = 1
    return(vect)
  }
  for(i in 1:num.teams) {
    lfs.a                  = rep(0, nCol)
    lfs.a[1:num.hitters]   = hitters.teams[,i]
    lfs.a[t.start:t.end]   = -1 * getIdx(i, num.teams)
    lfs.b                  = lfs.a
    lfs.b[t.start:t.end]   = -5 * getIdx(i, num.teams)
    
    constraints            = list.append(constraints, lfs.a)
    constraints            = list.append(constraints, lfs.b)
    
    model$sense            = append(model$sense, c(">", "<"))
    model$rhs              = append(model$rhs, c(0, 0))
  }
  team.size                = rep(0, nCol)
  team.size[t.start:t.end] = rep(1, num.teams)
  
  constraints              = list.append(constraints, team.size)
  
  model$sense              = append(model$sense, c("="))
  model$rhs                = append(model$rhs, c(2))
  
  # Players must come from at least two different games
  for(k in 1:num.games) {
    lfs         = append(players.games[,k], rep(0, num.teams))
    lfs         = append(lfs,  -1 * getIdx(k, num.games))
    
    constraints = list.append(constraints, lfs)
    
    model$sense = append(model$sense, c(">"))
    model$rhs   = append(model$rhs, c(0))
  }
  
  g.start                   = num.hitters + num.pitchers + num.teams + 1
  game.number               = rep(0, nCol)
  game.number[g.start:nCol] = rep(1, num.games)
  
  constraints               = list.append(constraints, game.number)
  
  model$sense               = append(model$sense, c(">"))
  model$rhs                 = append(model$rhs, c(2))
  
  # Pitcher stacking: no hitter and pitcher from opposing teams
  for(i in 1:num.pitchers) {
    lfs = append(pitchers.opponents[i,], 5 * getIdx(i, num.pitchers))
    lfs = append(lfs, rep(0, num.teams + num.games))
    
    constraints = list.append(constraints, lfs)
    
    model$sense = append(model$sense, c("<"))
    model$rhs   = append(model$rhs, c(5))
  }
  
  # Overlap constraint: maximum number of shared players between any two lineups
  for(i in 1:nrow(lineups)) {
    overlap.constraints = append(lineups[i,],
                                 rep(0, num.teams + num.games))
    
    constraints         = list.append(constraints, overlap.constraints)
    
    model$sense         = append(model$sense, c("<"))
    model$rhs           = append(model$rhs, c(num.overlap))
  }
  
  # Consecutive hitter stacking
  empty.matrix                               = matrix(rep(0, nCol * nCol), ncol = nCol)
  empty.matrix[1:num.hitters, 1:num.hitters] = consecutive.matrix
  sparse.consecutive.matrix                  = Matrix(empty.matrix, sparse = T)
  
  qc1           = list()
  qc1$Qc        = sparse.consecutive.matrix
  qc1$sense     = ">"
  qc1$rhs       = 20
  
  model$quadcon = list(qc1)
  
  # No pitcher can be chosen more than 50 times
  # for(i in 1:num.pitchers) {
  #   lhs                  = rep(0, nCol)
  #   lhs[num.hitters + i] = 1
  #   constraints          = list.append(constraints, lhs)
  #   model$sense          = append(model$sense, c("<"))
  #   model$rhs            = append(model$rhs, c(50 - sum(lineups[,(num.hitters + i)])))
  # }
  
  # Convert linear constraints into a sparse matrix
  constraints = Reduce(function(x, y) {
    rbind(x, y)
  }, constraints)
  
  model$A = Matrix(constraints, sparse = T)
  
  return(model)
}


## Solve model using Gurobi

## ------------------------------------------------------------ ##


solve.model = function(model, num.hitters,
                       num.pitchers, logToConsole) {
  params = list()
  params$LogToConsole = 0
  result = gurobi(model, params)
  if(logToConsole) {
    print(result$objval)
  }
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

create_lineups = function(num.lineups, num.overlap, formulation, objective,
                          salary.cap, hitters, pitchers, weight.a, weight.b, 
                          weight.c, weight.d, logToConsole) {
  
  # Number of hitters
  num.hitters    = nrow(hitters)
  
  # Number of pitchers
  num.pitchers   = nrow(pitchers)
  
  # Create a matrix where each row is a vector for a different position
  empty          = rep(0, num.hitters * 6)
  
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
  hitters.list = list(first.basemen  = hitters.matrix[1,],
                      second.basemen = hitters.matrix[2,],
                      third.basemen  = hitters.matrix[3,],
                      shortstops     = hitters.matrix[4,],
                      outfielders    = hitters.matrix[5,],
                      catchers       = hitters.matrix[6,])
  
  # List where each entry is a vector that stores info on what team a player is on
  teams     = unique(hitters[,"Team"])
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
  players    = rbind(subset(hitters, select = -c(Order)), pitchers)
  games      = unique(players[,"Teams.Playing"])
  
  duplicated = c()
  for(game in games) {
    game = unlist(strsplit(game, split = "@"))
    if(length(which(grepl(game[1], duplicated) & 
                    grepl(game[2], duplicated))) > 0) {
      games      = games[games != paste(game[1], game[2], sep = "@")]
    } else {
      duplicated = append(duplicated, paste(game[1], game[2], sep = "@"))
    }
  }
  
  num.games          = length(games)
  games.distribution = list()
  
  for(i in 1:nrow(players)) {
    player.info = rep(0, num.games) 
    team        = unlist(strsplit(players[i, "Teams.Playing"], split = "@"))
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
  opponents      = pitchers[,"Opponent"]
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
  
  empty              = rep(0, num.hitters * num.hitters)
  consecutive.matrix = matrix(empty, nrow = num.hitters)

  for(i in 1:num.hitters) {
    for(j in 1:num.hitters) {
      consecutive.matrix[i, j] = ifelse(is.consecutive(hitters, i, j), 1, 0)
    }
  }
  
  
  # Create a lineup
  tracer      = matrix(rep(0, num.hitters + num.pitchers), nrow = 1)
  basic.model = formulation(hitters, pitchers, tracer, num.overlap,
                            num.hitters, num.pitchers, hitters.list[[1]],
                            hitters.list[[2]], hitters.list[[3]],
                            hitters.list[[4]], hitters.list[[5]],
                            hitters.list[[6]], num.teams, hitters.teams,
                            num.games, hitters.games, players.games,
                            salary.cap, pitchers.opponents, consecutive.matrix)
  obj.model   = objective(basic.model, hitters, pitchers,
                          num.teams, num.games, weight.a,
                          weight.b, weight.c, weight.d)
  lineups     = solve.model(obj.model, num.hitters,
                            num.pitchers, logToConsole)
  lineups     = matrix(lineups, nrow = 1)
  
  if(num.lineups > 1) {
    for(i in 1:(num.lineups - 1)) {
      basic.model = formulation(hitters, pitchers, lineups, num.overlap,
                                num.hitters, num.pitchers, hitters.list[[1]],
                                hitters.list[[2]], hitters.list[[3]],
                                hitters.list[[4]], hitters.list[[5]],
                                hitters.list[[6]], num.teams, hitters.teams,
                                num.games, hitters.games, players.games,
                                salary.cap, pitchers.opponents, consecutive.matrix)
      obj.model   = objective(basic.model, hitters, pitchers,
                              num.teams, num.games, weight.a,
                              weight.b, weight.c, weight.d)
      lineup      = solve.model(obj.model, num.hitters,
                                num.pitchers, logToConsole)
      lineups     = rbind(lineups, lineup)
    }
  }
  return(lineups)
}


## Takes the lineups matrix and writes it to a CSV file, where each
## row is a lineup

## ------------------------------------------------------------ ##


lineups.to.csv = function(lineups, hitters, pitchers, path.output) {
  for(i in 1:nrow(lineups)) {
    lineup           = lineups[i,]
    chosen.hitters   = lineup[1:nrow(hitters)]
    chosen.pitchers  = lineup[(nrow(hitters) + 1):length(lineup)]
    
    hitters.indices  = which(chosen.hitters > 0.5)
    pitchers.indices = which(chosen.pitchers > 0.5)
    
    name             = append(c(hitters[hitters.indices, "Name"]),
                              c(pitchers[pitchers.indices, "Name"]))
    
    names            = append(name,
                              c(hitters[hitters.indices, "Team"]))
    
    names            = append(names,
                              c(pitchers[pitchers.indices, "Team"]))
    
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
  points = rep(0, nrow(lineups))
  
  for(i in 1:nrow(lineups)) {
    lineup           = lineups[i,]
    chosen.hitters   = lineup[1:nrow(hitters)]
    chosen.pitchers  = lineup[(nrow(hitters) + 1):length(lineup)]
    
    hitters.indices  = which(chosen.hitters > 0.5)
    pitchers.indices = which(chosen.pitchers > 0.5)
    
    points[i]        = sum(hitters[hitters.indices, "Actual"]) +
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
  max.scores  = list()
  saber.files = list.files(path.saber)[360:460]
  dates       = lapply(saber.files, function(x) {
                       unlist(strsplit(gsub("[A-z\\.]", "", x), 
                                       split = " "))[1] })
  for(overlap in overlaps) {
    for(d in 1:length(dates)) {
      tryCatch({
        date          = dates[[d]]
        saber.file    = paste(path.saber, saber.files[[d]], sep = "/")
        if(dim(read.csv(saber.file))[1] < 190) {
          stop("Dataset too small!")
        }
        hitters.temp  = gsub.custom(path.hitters.proj, date)
        pitchers.temp = gsub.custom(path.pitchers.proj, date)
        
        hitters.proj  = merge.rotogrinders(hitters.temp, saber.file, TRUE)
        pitchers.proj = merge.rotogrinders(pitchers.temp, saber.file, FALSE)
        
        for(a in c(0.2, 0.4, 0.6, 0.8)) {
          for(b in c(0.2, 0.4, 0.6, 0.8)) {
            df     = create_lineups(num.lineups, num.overlap, setup.model, objective.g,
                                    salary.cap, hitters.proj, pitchers.proj, a, b, 
                                    0, 0, F)
            
            scores = get.scores(df, hitters.proj, pitchers.proj)
            print(paste(toString(date), paste(toString(a), toString(b), sep = "//"), sep = "//"))
            print(max(scores))
          }
        }

        max.scores = list.append(max.scores, name = df)
        names(max.scores)[which(names(max.scores) == "name")] =
          paste(toString(dim(read.csv(saber.file))[1]), toString(date), sep = "//")
      }, error = function(e) {})
    }
  }
  return(max.scores)
}
