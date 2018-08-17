source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_class.R")


## Outputs Rotogrinders projection dataframe

## ------------------------------------------------------------ ##


clean.rotogrinders.hitters = function(roto.path) {
  df = read.csv(roto.path,
                stringsAsFactors = F)
  df$Hands = as.factor(paste(df$Hand, df$PHND, sep = ""))
  df$Away = as.factor(ifelse(grepl("@", df$Opp), "Away", "Home"))
  df = subset(df, select = -c(Hand, Pitcher, PHND,
                              PLTN., Rank, Average, pOWN.,
                              ContR, X, X.1, Pt...K))
  names(df)[which(names(df) == "Opp")] = "Opponent"
  names(df)[which(names(df) == "Points")] = "Roto.Projection"
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


clean.saber = function(path.saber.file) {
  df = read.csv(path.saber.file,
                stringsAsFactors = F)
  df = subset(df, 
              select = c(Name, Projection, Actual,
                         dk_95_percentile, PA, Singles,
                         Doubles, Triples, HR, R, RBI,
                         SB, CS))
  names(df)[which(names(df) == "Projection")] = "Saber.Projection"
  return(df)
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
  swish.df = subset(swish.df,
                    select = c(Name, Swish.Projection))
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
    }, error = function(e) {print(e)})
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


## Store all cleaned dataframes for hitters in environment by calling the 
## above functions

## ------------------------------------------------------------ ##

nerd.df = nerd.combine(path.nerd)
roto.df = roto.combine(path.roto.hitters,
                       TRUE)
swish.df = swish.combine(path.swish)
saber.df = saber.combine(path.saber)

combined.df = merge(roto.df, nerd.df,
                    by = c("Name", "Date"),
                    all.x = T)

combined.df = merge(combined.df, saber.df,
                    by = c("Name", "Date"),
                    all.x = T)

combined.df = merge(combined.df, swish.df,
                    by = c("Name", "Date"),
                    all.x = T)

combined.df = combined.df[-which(is.na(combined.df$Actual)),]


## Additional "standard error" feature

## ------------------------------------------------------------ ##


combined.df$Standard.Error = combined.df$dk_95_percentile - combined.df$Saber.Projection


## Dataset for modeling

## ------------------------------------------------------------ ##


model.df = subset(combined.df,
                  select = -c(Team,
                              Position, Opponent,
                              Teams.Playing, Order))

model.df$Date = as.Date(model.df$Date)
model.df = model.df[model.df$Date > as.Date("2018-05-30"),]
model.df$Date = as.numeric(model.df$Date - min(model.df$Date))

model.df$Salary = model.df$Salary/10000
model.df$SalDiff = model.df$SalDiff/1000
model.df$RankDiff = model.df$RankDiff/100

train.df = model.df[model.df$Date < 60,]
test.df = model.df[model.df$Date >= 60,]


## Multiple imputation on testing and training data

## ------------------------------------------------------------ ##


temp.data.train = mice(train.df, method = "pmm")
temp.data.test = mice(test.df, method = "pmm")
train.df = mice::complete(temp.data.train)
test.df = mice::complete(temp.data.test)


## Forward selection and Cook's distance filtering

## ------------------------------------------------------------ ##


null = lm(Actual ~ 1, data = train.df)
full = lm(Actual ~ ., data = train.df)

both.select = step(full, 
                   scope = list(lower = null, upper = full), 
                   direction = "both")

forward.select = step(null, 
                      scope = list(lower = null, upper = full), 
                      direction = "forward")

final.formula = "Actual ~ Salary + SalDiff + Total + ISO + SLG + AB.1 + ISO.1 + 
    SLG.1 + AVG.2 + wOBA.2 + AB.3 + AVG.3 + OBP.3 + K..3 + Last.5.Avg + 
    Season.Ceiling + Saber.Projection + CS + Swish.Projection"

forward.formula = "Actual ~ Swish.Projection + Last.5.Avg + Saber.Projection + Total + 
    CS + ISO + K..3 + Season.Ceiling + SalDiff + Salary"

lm.both = lm(as.formula(final.formula), data = train.df)
lm.fwd = lm(as.formula(forward.formula), data = train.df)


## First layer models

## ------------------------------------------------------------ ##


# Generalized additive model

train.df$Name = as.factor(train.df$Name)


mlb.gam = gam(Actual ~ s(Swish.Projection, by = Away) + s(Last.5.Avg) + s(Saber.Projection, by = Away) + 
                Total + CS + ISO + s(K..3) + Season.Ceiling + SalDiff + Salary + s(Date, bs = "re")
                te(Season.Ceiling, Saber.Projection) + te(SalDiff, Last.5.Avg),
              data = train.df,
              method = "REML")

# Linear mixed effects model

mlb.lme = lme(Actual ~ Swish.Projection + Last.5.Avg + Saber.Projection + Total + 
                 CS + ISO + K..3 + Season.Ceiling + SalDiff + Salary, 
              random = ~ 1 | Name,
               data = train.df)

mse = function(df, projected) {
  for(i in 1:length(projected)) {
    if(is.na(projected[i])) {
      projected[i] = df$Swish.Projection[i]
    }
  }
  diff = df$Actual - projected
  diff.sq = diff^2
  return(sum(diff.sq))
}


training.set.a = which(train.df$Date <= 45)
training.set.b = which(train.df$Date <= 50)
training.set.c = which(train.df$Date <= 55)

holdout.set.a = which(train.df$Date > 45 & train.df$Date < 50)
holdout.set.b = which(train.df$Date > 50 & train.df$Date < 55)
holdout.set.c = which(train.df$Date > 55 & train.df$Date < 60)


fit.on = list(rs1 = training.set.a,
              rs2 = training.set.b,
              rs3 = training.set.c)

pred.on = list(rs1 = holdout.set.a,
               rs2 = holdout.set.b,
               rs3 = holdout.set.c)


control = trainControl(method = "cv",
                       index = fit.on,
                       indexOut = pred.on,
                       verboseIter = T,
                       summaryFunction = defaultSummary)

labelName = "Actual"
predictors = c("Salary", "SalDiff", "Total", "ISO", "SLG", "AB.1", "ISO.1",
  "SLG.1", "AVG.2", "wOBA.2", "AB.3", "AVG.3", "OBP.3", "K..3", "Last.5.Avg",
  "Season.Ceiling", "Saber.Projection", "CS", "Swish.Projection", "Name")


model.gbm = train(as.matrix(train.df[,predictors]),
                  train.df[,labelName],
                  method = "gbm",
                  trControl = control)

mixed.boost = metb(as.matrix(train.df[,predictors]),
                   train.df[,labelName],
                   id = "Name",
                   n.trees = 50,
                   interaction.depth = 3,
                   shrinkage = 0.1,
                   n.minobsinnode = 10)
