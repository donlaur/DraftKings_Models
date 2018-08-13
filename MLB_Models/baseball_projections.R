source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_class.R")


## Outputs Rotogrinders projection dataframe

## ------------------------------------------------------------ ##


clean.rotogrinders.hitters = function(roto.path) {
  df = read.csv(roto.path,
                stringsAsFactors = F)
  df$Hands = as.factor(paste(df$Hand, df$PHND, sep = ""))
  df$Away = as.factor(ifelse(grepl("@", df$Opp), "Away", "Home"))
  df = subset(df, select = c(Name, Salary,
                             Team, Position,
                             Opp, Hands, Away, Order,
                             SalDiff, RankDiff,
                             O.U, Line, Total,
                             Movement, AB, AVG,
                             wOBA, ISO, Points))
  names(df) = c("Name", "Salary",
                "Team", "Position",
                "Opponent", "Hands", "Location",
                "Order", "SalDiff",
                "RankDiff", "O.U",
                "Line", "Total",
                "Movement", "AB",
                "AVG", "wOBA", "ISO",
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
  df$SalDiff = suppressWarnings(as.numeric(df$SalDiff))
  df$RankDiff = suppressWarnings(as.numeric(df$RankDiff))
  df$Movement = suppressWarnings(as.numeric(df$Movement))
  df$Hands = as.factor(df$Hands)
  return(df)
}

clean.rotogrinders.pitchers = function(roto.path) {
  df = read.csv(roto.path,
                stringsAsFactors = F)
  df$Away = as.factor(ifelse(grepl("@", df$Opp), "Away", "Home"))
  df = subset(df, select = c(Name, Salary,
                             Team, Position,
                             Opp, Hand, Away, 
                             SalDiff, RankDiff,
                             O.U, Line, Total,
                             Movement, xL, LwOBA,
                             LISO, LK.9, xR, RwOBA,
                             RISO, RK.9, GP, SIERA,
                             xFIP, HR.FB, xWOBA, xK.9,
                             Points))
  names(df) = c("Name", "Salary",
                "Team", "Position",
                "Opponent", "Hand", "Location",
                "SalDiff", "RankDiff", "O.U",
                "Line", "Total",
                "Movement", "xL",
                "LwOBA", "LISO", "LK.9",
                "xR", "RwOBA", "RISO",
                "RK.9", "GP", "SIERA",
                "xFIP", "HR.FB", "xWOBA",
                "xK.9", "Projection")
  df$Salary = gsub(c("K"), "", df$Salary)
  df$Salary = gsub(c("\\$"), "", df$Salary)
  df$Salary = as.numeric(df$Salary) * 1000
  df$Team = gsub("@", "", df$Team)
  df$Team = gsub(" ", "", df$Team)
  df$Opponent = gsub("@", "", df$Opponent)
  df$Opponent = gsub(" ", "", df$Opponent)
  df$Teams.Playing = paste(df$Team, df$Opponent, sep = "@")
  df$Teams.Playing = gsub(" ", "", df$Teams.Playing)
  df$SalDiff = suppressWarnings(as.numeric(df$SalDiff))
  df$RankDiff = suppressWarnings(as.numeric(df$RankDiff))
  df$Movement = suppressWarnings(as.numeric(df$Movement))
  return(df)
}

roto.combine = function(path.roto, is.hitter) {
  setwd(path.roto)
  dates = lapply(list.files(), function(x) {
    gsub("[A-z\\.]", "", x)
  })
  players.files = list()
  if(is.hitter) {
    for(file in list.files()) {
      tryCatch ({
        players.files = list.append(players.files,
                                    clean.rotogrinders.hitters(file))
      }, error = function(e) {})
    }
  } else {
    for(file in list.files()) {
      tryCatch ({
        players.files = list.append(players.files,
                                    clean.rotogrinders.pitchers(file))
      }, error = function(e) {})
    }
  }
  for(i in 1:length(players.files)) {
    players.files[[i]]$Date = dates[[i]]
  }
  
  roto.df = Reduce(function(x, y) {
    rbind(x, y)
  }, players.files)
  
  roto.df = roto.df[-which(duplicated(roto.df)),]
  
  return(roto.df)
}


## Outputs SaberSim projection dataframe

## ------------------------------------------------------------ ##


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
  
  saber.df = saber.df[-which(duplicated(saber.df)),]
  saber.df = saber.df[-which(is.na(saber.df$Actual)),]
  
  return(saber.df)
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
                      "Swish.Projection",
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


clean.nerd = function(path.nerd.file) {
  nerd.df = read.csv(path.nerd.file,
                     stringsAsFactors = F,
                     header = F)
  colnames(nerd.df) = c("Name",
                        "Last.5.Avg",
                        "Season.Avg",
                        "Season.Floor",
                        "Season.Ceiling",
                        "Nerd.Projection",
                        "None")
  nerd.df = subset(nerd.df,
                   select = -c(None))
}

nerd.combine = function(path.nerd) {
  setwd(path.nerd)
  players.filenames = list.files()
  players.filenames = players.filenames[grepl("nerd", players.filenames)]
  players.files = list()
  dates = lapply(players.filenames, function(x) {
    unlist(strsplit(gsub("[A-z\\.]", "", x), split = " "))[1]
  })
  
  for(file in players.filenames) {
    tryCatch ({
      players.files = list.append(players.files,
                                  clean.nerd(file))
    }, error = function(e) {})
  }
  
  for(i in 1:length(players.files)) {
    players.files[[i]]$Date = dates[[i]]
  }
  
  nerd.df = Reduce(function(x, y) {
    rbind(x, y)
  }, players.files)
  
  nerd.df = nerd.df[complete.cases(nerd.df),]
  
  return(nerd.df)
}


## Store all cleaned dataframes in environment by calling the above functions

## ------------------------------------------------------------ ##

nerd.df = nerd.combine(path.nerd)
roto.df = roto.combine(path.roto.hitters,
                       path.roto.pitchers)
swish.df = swish.combine(path.swish)
saber.df = saber.combine(path.saber)

combined.df = merge(roto.df, nerd.df,
                    by = c("Name", "Date"))

saber.df = subset(saber.df, 
                  select = -c(Team, Opponent,
                              Position, Salary))

combined.df = merge(combined.df, saber.df,
                    by = c("Name", "Date"))

swish.df = subset(swish.df,
                  select = -c(Position, Salary, 
                              Teams.Playing, Team, 
                              Opponent))

combined.df = merge(combined.df, swish.df,
                    by = c("Name", "Date"))


## Which website gives the best predictions?

## ------------------------------------------------------------ ##


mse = function(actual, projected) {
  diff = actual - projected
  diff.sq = diff^2
  return(sum(diff.sq))
}

mse.roto = mse(combined.df$Actual, combined.df$Roto.Projection)
mse.nerd = mse(combined.df$Actual, combined.df$Nerd.Projection)
mse.swish = mse(combined.df$Actual, combined.df$Swish.Projection)
mse.saber = mse(combined.df$Actual, combined.df$Saber.Projection)
mse.average = mse(combined.df$Actual, combined.df$Season.Avg)
mse.25 = mse(combined.df$Actual, combined.df$Projection_25)
mse.50 = mse(combined.df$Actual, combined.df$Projection_50)
mse.75 = mse(combined.df$Actual, combined.df$Projection_75)

mse.df = data.frame(Roto.Error = mse.roto,
                    Nerd.Error = mse.nerd,
                    Swish.Error = mse.swish,
                    Saber.Error = mse.saber,
                    Average.Error = mse.average,
                    Error.25 = mse.25,
                    Error.50 = mse.50,
                    Error.75 = mse.75)

print(mse.df)


## Additional "standard error" feature

## ------------------------------------------------------------ ##


combined.df$Standard.Error = combined.df$Projection_85 - combined.df$Saber.Projection


## Dataset for modeling

## ------------------------------------------------------------ ##


model.df = subset(combined.df,
                  select = -c(Name, Date, Team,
                              Position, Opponent,
                              Teams.Playing))

model.df$Salary = model.df$Salary/1000


## Examining the dataset

## ------------------------------------------------------------ ##


M = cor(model.df)
corrplot(M, method = "circle")


## Forward selection

## ------------------------------------------------------------ ##


null = lm(Actual ~ 1, data = model.df)
full = lm(Actual ~ ., data = model.df)

both.select = step(null, 
                   scope = list(lower = null, upper = full), 
                   direction = "both")

selected.variables = c("Swish.Projection", "Last.5.Avg", "Saber.Projection",
                       "Season.Avg", "Roto.Projection", "Salary", "Value")


## Setting up data for ML

## ------------------------------------------------------------ ##


samples = sample(nrow(model.df), floor(nrow(model.df) * 0.8))
train.df = model.df[samples,]
test.df = model.df[-samples,]

cores = detectCores()
cluster = makePSOCKcluster(cores)
registerDoParallel(cluster)

control = trainControl(method = "cv",
                       number = 10,
                       allowParallel = T,
                       verboseIter = T,
                       summaryFunction = defaultSummary)

label = "Actual"
all.predictors = names(train.df)[names(train.df) != label]

model.a = train(train.df[,all.predictors],
                train.df[,label],
                method = "rf",
                trControl = control)

model.b = train(train.df[,selected.variables],
                train.df[,label],
                method = "rf",
                trControl = control)

model.c = train(train.df[,all.predictors],
                train.df[,label],
                method = "rf",
                preProcess = c("center", "scale"),
                trControl = control)

model.d = train(train.df[,selected.variables],
                train.df[,label],
                method = "rf",
                preProcess = c("pca"),
                trControl = control)