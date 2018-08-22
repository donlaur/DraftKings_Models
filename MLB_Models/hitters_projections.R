## Playground for testing hitter models

source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_class.R")
source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_dataset.R")


## Creating the training dataset

## ------------------------------------------------------------ ##


start.date = "2018-05-01"
end.date = "2018-07-31"

train.df = create.dataset(path.nerd, path.roto.hitters,
                          path.swish, path.saber,
                          start.date, end.date, TRUE)
train.df = impute.data(train.df)

test.df = create.dataset(path.nerd, path.roto.hitters,
                         path.swish, path.saber,
                         "2018-08-01", "2018-08-04", TRUE)

test.df = impute.data(test.df)


## Feature selection

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

mse = function(df, projected) {
  for(i in 1:length(projected)) {
    if(is.na(projected[i])) {
      projected[i] = df$Saber.Projection[i]
    }
  }
  diff = df$Actual - projected
  diff.sq = diff^2
  return(sum(diff.sq))
}

## First layer models

## ------------------------------------------------------------ ##


# Generalized additive model

train.df$Name = as.factor(train.df$Name)

mlb.gam = gam(Actual ~ s(Swish.Projection, by = Away) + s(Last.5.Avg) + s(Saber.Projection, by = Away) + Total
              + CS + ISO + s(K..3) + Season.Ceiling + SalDiff + Salary
              + te(Season.Ceiling, Saber.Projection) + te(SalDiff, Last.5.Avg),
              data = train.df,
              method = "REML")

# Linear mixed effects model

mlb.lme = lme(Actual ~ Swish.Projection + Last.5.Avg + Saber.Projection + Total + 
                CS + ISO + K..3 + Season.Ceiling + SalDiff + Salary, 
              random = ~ 1 | Name,
              data = train.df)

# Mixed effects tree

mlb.reem = REEMtree(Actual ~ Swish.Projection + Last.5.Avg + Saber.Projection + Total + 
                      CS + ISO + K..3 + Season.Ceiling + SalDiff + Salary,
                    data = train.df,
                    random = ~1 | Name)


# Mixed effects gradient boosted tree

y = train.df$Actual
X = train.df %>% select(Salary, SalDiff, Total, ISO, SLG, AB.1, ISO.1, SLG.1, AVG.2,
                    wOBA.2, AB.3, AVG.3, OBP.3, K..3, Last.5.Avg, Season.Ceiling, 
                    Saber.Projection, CS, Swish.Projection, Name) %>% 
  mutate_if(is.character, as.factor)

test = test.df %>% select(Salary, SalDiff, Total, ISO, SLG, AB.1, ISO.1, SLG.1, AVG.2,
                        wOBA.2, AB.3, AVG.3, OBP.3, K..3, Last.5.Avg, Season.Ceiling, 
                        Saber.Projection, CS, Swish.Projection, Name) %>% 
  mutate_if(is.character, as.factor)


out = metb(y=y, X=X, id="Name", 
            n.trees=50,
            shrinkage=.1, 
            interaction.depth=3,
            num_threads=8,
            save.mods = T)
