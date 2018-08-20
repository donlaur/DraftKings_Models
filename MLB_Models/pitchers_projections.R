source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_class.R")
source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_dataset.R")


start.date = "2018-05-01"
end.date = "2018-07-31"

train.df = create.dataset(path.nerd, path.roto.pitchers,
                          path.swish, path.saber,
                          start.date, end.date, FALSE)

train.df = impute.data(train.df)

null = lm(Actual ~ 1, data = train.df)
full = lm(Actual ~ ., data = train.df)

both.select = step(full, 
                   scope = list(lower = null, upper = full), 
                   direction = "both")

forward.select = step(null, 
                      scope = list(lower = null, upper = full), 
                      direction = "forward")

O.U + Total + LISO + xFIP + xWOBA + Season.Avg + Season.Ceiling + 
  Swish.Projection