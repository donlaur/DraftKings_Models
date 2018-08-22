## Change these: paths to folders containing data and to output 

## ------------------------------------------------------------ ##


path.roto.hitters = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Roto_Hitters"
path.roto.pitchers = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Roto_Pitchers"
path.saber = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Saber_Sim"
path.swish = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Swish_Analytics"
path.nerd = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Fantasy_Nerd"
output = "C:/Users/Ming/Documents/Fantasy_Models/output"
path.output = "C:/Users/Ming/Documents/Fantasy_Models/output/MLB_consecutive.csv"


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
  df$Hand = as.factor(df$Hand)
  df = subset(df, select = -c( Rank, Average, pOWN.,
                               ContR, X, X.1, X.2, X.3,
                               X.4, X.5, X.6, X.7))
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


## Store all cleaned dataframes for hitters in environment by calling the 
## above functions

## ------------------------------------------------------------ ##

create.dataset = function(path.nerd, path.roto,
                          path.swish, path.saber,
                          start.date, end.date,
                          is.hitter) {
  nerd.df = nerd.combine(path.nerd)
  roto.df = roto.combine(path.roto,
                         is.hitter)
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
  combined.df$Standard.Error = combined.df$dk_95_percentile - combined.df$Saber.Projection
  
  model.df = subset(combined.df,
                    select = -c(Team,
                                Position, Opponent,
                                Teams.Playing))
  
  if(is.hitter) {
    model.df = subset(model.df, select = -c(Order))
  }
  
  model.df$Date = as.Date(model.df$Date)
  model.df = model.df[model.df$Date >= as.Date(start.date) & model.df$Date < as.Date(end.date),]
  model.df$Date = as.numeric(model.df$Date - min(model.df$Date))
  
  model.df$Salary = model.df$Salary/10000
  model.df$SalDiff = model.df$SalDiff/1000
  model.df$RankDiff = model.df$RankDiff/100
  
  return(model.df)
}

create.dataset.b = function(saber.df,
                            path.nerd, 
                            path.roto,
                            path.swish,
                            start.date,
                            date,
                            is.hitter) {
  nerd.path = which(grepl(date, list.files(path.nerd)))
  nerd.path = list.files(path.nerd)[nerd.path]
  
  roto.path = which(grepl(date, list.files(path.roto)))
  roto.path = list.files(path.roto)[roto.path]
  
  swish.path = which(grepl(date, list.files(path.swish)))
  swish.path = list.files(path.swish)[swish.path]
  
  swish.df = clean.swish(paste(path.swish, swish.path, sep = "/"))
  nerd.df = clean.nerd(paste(path.nerd, nerd.path, sep = "/"))
  roto.df = data.frame()
  
  if(is.hitter) {
    roto.df = clean.rotogrinders.hitters(paste(path.roto, roto.path, sep = "/"))
  } else {
    roto.df = clean.rotogrinders.pitchers(paste(path.roto, roto.path, sep = "/"))
  }

  combined.df = merge(saber.df, nerd.df,
                      all.x = T,
                      by = "Name")
  combined.df = merge(combined.df, swish.df,
                      all.x = T,
                      by = "Name")
  combined.df = merge(combined.df, roto.df,
                      all.x = T,
                      by = "Name")
  
  combined.df = combined.df[,colSums(is.na(combined.df)) < nrow(combined.df)]
  combined.df = subset(combined.df,
                       select = -c(Team, Position,
                                   Opponent, Teams.Playing))
  
  combined.df$Date = as.numeric(as.Date(date) - as.Date(start.date))
  
  combined.df$Salary = combined.df$Salary/10000
  combined.df$SalDiff = combined.df$SalDiff/1000
  combined.df$RankDiff = combined.df$RankDiff/100
  
  return(combined.df)
}

## Multiple imputation on testing and training data

## ------------------------------------------------------------ ##


impute.data = function(df) {
  print("Imputing data...")
  temp = mice(df, method = "pmm", print = F)
  df = mice::complete(temp)
  return(df)
}


## Return predictions

## ------------------------------------------------------------ ##


to.formula = function(vars) {
  predictors = reduce(vars, 
                      function(x,y) paste(x,y,sep="+"))
  return(paste("Actual ~ ", predictors, sep = ""))
}

gbm.predict.hitter = function(train.df, test.df) {
  y = train.df$Actual
  X = train.df %>% select(Salary, SalDiff, Total, ISO, SLG, AB.1, ISO.1, SLG.1, AVG.2,
                          wOBA.2, AB.3, AVG.3, OBP.3, K..3, Last.5.Avg, Season.Ceiling, 
                          Saber.Projection, CS, Name) %>% 
    mutate_if(is.character, as.factor)
  
  test = test.df %>% select(Salary, SalDiff, Total, ISO, SLG, AB.1, ISO.1, SLG.1, AVG.2,
                            wOBA.2, AB.3, AVG.3, OBP.3, K..3, Last.5.Avg, Season.Ceiling, 
                            Saber.Projection, CS, Name) %>% 
    mutate_if(is.character, as.factor)
  
  print("Generating hitter predictions...")
  out = metb(y=y, X=X, id="Name", 
             n.trees=50,
             shrinkage=.1, 
             interaction.depth=3,
             num_threads=8,
             save.mods = T)
  
  predictions = predict(out, test, id = "Name")
  for(i in 1:length(predictions)) {
    if(is.na(predictions[i])) {
      predictions[i] = test.df$Saber.Projection[i]
    }
  }
  return(predictions[[1]])
}


lme.predict.pitcher = function(train.df, test.df) {
  mlb.lme = lme(Actual ~ Salary + LISO + RwOBA + xFIP + xWOBA + Pt...K + xL.1 + 
                  LwOBA.1 + LISO.1 + RISO.1 + GP.1 + xFIP.1 + LwOBA.2 + RK.9.2 + 
                  xWOBA.2 + xK.9.2 + LwOBA.3 + RwOBA.3 + RK.9.3 + SIERA.3 + 
                  Away + Season.Avg + Season.Ceiling + Saber.Projection, 
                random = ~ 1 | Name,
                data = train.df)
  print("Generating pitcher predictions...")
  predictions = predict(mlb.lme, test.df)
  for(i in 1:length(predictions)) {
    if(is.na(predictions[i])) {
      predictions[i] = test.df$Saber.Projection[i]
    }
  }
  return(predictions)
}
