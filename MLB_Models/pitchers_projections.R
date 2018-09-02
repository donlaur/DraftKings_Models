## Playground for testing pitcher models

source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_class.R")
source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_dataset.R")


## Creating the training dataset

## ------------------------------------------------------------ ##


start.date = "2018-05-01"
end.date = "2018-07-31"

train.df = create.dataset(path.nerd, path.roto.pitchers,
                          path.swish, path.saber,
                          start.date, end.date, FALSE)

train.df = impute.data(train.df)

test.df = create.dataset(path.nerd, path.roto.pitchers,
                         path.swish, path.saber,
                         "2018-08-01", "2018-08-04", FALSE)

test.df = impute.data(test.df)

## Feature selection

## ------------------------------------------------------------ ##


null = lm(Actual ~ 1, data = train.df)
full = lm(Actual ~ .-Name-Swish.Projection, data = train.df)

both.select = step(full, 
                   scope = list(lower = null, upper = full), 
                   direction = "both")

forward.select = step(null, 
                      scope = list(lower = null, upper = full), 
                      direction = "both")

terms = names(both.select$coefficients)
terms = terms[2:length(terms)]
terms = append(terms, c("Name"))

fwd.terms = names(forward.select$coefficients)


lm1 = lm(Actual ~ Salary + LISO + RwOBA + xFIP + xWOBA + Pt...K + xL.1 + 
            LwOBA.1 + LISO.1 + RISO.1 + GP.1 + xFIP.1 + LwOBA.2 + RK.9.2 + 
            xWOBA.2 + xK.9.2 + LwOBA.3 + RwOBA.3 + RK.9.3 + SIERA.3 + 
            Away + Season.Avg + Season.Ceiling + Saber.Projection + Five.Day.Trend,
          data = train.df)

lm2 = lm(Actual ~ Salary + O.U + Total + LISO + RwOBA + GP + xFIP + xWOBA + 
           Pt...K + xL.1 + LwOBA.1 + LISO.1 + RISO.1 + GP.1 + xFIP.1 + 
           LwOBA.2 + RK.9.2 + xWOBA.2 + xK.9.2 + LwOBA.3 + RwOBA.3 + 
           RK.9.3 + GP.3 + SIERA.3 + Away + Season.Avg + Season.Ceiling + 
           Saber.Projection + Total:Away,
          data = train.df)

anova(lm1, lm2, test = "F")

## First layer models

## ------------------------------------------------------------ ##


# Mixed effects linear model

mlb.lme = lme(Actual ~ Salary + LISO + RwOBA + xFIP + xWOBA + Pt...K + xL.1 + 
                LwOBA.1 + LISO.1 + RISO.1 + GP.1 + xFIP.1 + LwOBA.2 + RK.9.2 + 
                xWOBA.2 + xK.9.2 + LwOBA.3 + RwOBA.3 + RK.9.3 + SIERA.3 + 
                Away + Season.Avg + Season.Ceiling + Saber.Projection, 
              random = ~ 1 | Name,
              data = train.df)

mlb.lme2 = lme(Actual ~ Salary + O.U + Total + LISO + RwOBA + GP + xFIP + xWOBA + 
                 Pt...K + xL.1 + LwOBA.1 + LISO.1 + RISO.1 + GP.1 + xFIP.1 + 
                 LwOBA.2 + RK.9.2 + xWOBA.2 + xK.9.2 + LwOBA.3 + RwOBA.3 + 
                 RK.9.3 + GP.3 + SIERA.3 + Away + Season.Avg + Season.Ceiling + 
                 Saber.Projection + Diff.1 + Diff.2 + Diff.3 + Diff.4 + Five.Day.Trend,
               random = ~ 1 |Name,
               data = train.df)

# Mixed effects tree

mlb.reem = REEMtree(Actual ~ Salary + LISO + RwOBA + xFIP + xWOBA + Pt...K + xL.1 + 
                      LwOBA.1 + LISO.1 + RISO.1 + GP.1 + xFIP.1 + LwOBA.2 + RK.9.2 + 
                      xWOBA.2 + xK.9.2 + LwOBA.3 + RwOBA.3 + RK.9.3 + SIERA.3 + 
                      Away + Season.Avg + Season.Ceiling + Saber.Projection + 
                      Name,
                    data = train.df,
                    random = ~1 | Name)

temp = test.df[test.df$Name %in% train.df$Name,]

# Mixed effects gradient boosting

y = train.df$Actual
X = train.df %>% select(terms) %>% 
  mutate_if(is.character, as.factor)

test = test.df %>% select(terms) %>% 
  mutate_if(is.character, as.factor)


for(depth in c(2, 4, 6)) {
  out = metb(y=y, X=X, id="Name", 
             n.trees=50,
             shrinkage=0, 
             interaction.depth=depth,
             num_threads=8,
             save.mods = T)
  print(paste(paste(toString(tree), toString(shrinkage), sep = "//"), toString(depth), sep = "//"))
  prediction = predict(out, test, id = "Name")
  print(mse(test.df, prediction[[1]]))
}

# Bayesian mixed effects model

blme.mod = blmer(Actual ~ Salary + LISO + RwOBA + xFIP + xWOBA + Pt...K + xL.1 + 
                  LwOBA.1 + LISO.1 + RISO.1 + GP.1 + xFIP.1 + LwOBA.2 + RK.9.2 + 
                  xWOBA.2 + xK.9.2 + LwOBA.3 + RwOBA.3 + RK.9.3 + SIERA.3 + 
                  Away + Season.Avg + Season.Ceiling + Saber.Projection + (1|Name),
                 REML = FALSE,
                data = train.df)

# Second layer model

train.df$Linear.Prediction = predict(mlb.lme, train.df)
test.df$Linear.Prediction = predict(mlb.lme, test.df)

for(i in 1:nrow(test.df)) {
  if(is.na(test.df[i, "Linear.Prediction"])) {
    test.df[i, "Linear.Prediction"] = test.df[i, "Saber.Projection"]
  }
}

test.a = which(train.df$Date < 85)
test.b = which(train.df$Date < 90)
test.c = which(train.df$Date < 95)

holdout.a = which(train.df$Date >= 85 & train.df$Date < 90)
holdout.b = which(train.df$Date >= 90 & train.df$Date < 95)
holdout.c = which(train.df$Date >= 95 & train.df$Date < 100)

fit.on = list(rs1 = test.a,
              rs2 = test.b,
              rs3 = test.c)

pred.on = list(rs1 = holdout.a,
               rs2 = holdout.b,
               rs3 = holdout.c)

cores = detectCores()
cluster = makePSOCKcluster(cores)
registerDoParallel(cluster)

control = trainControl(method = "cv",
                       index = fit.on,
                       indexOut = pred.on,
                       allowParallel = T,
                       verboseIter = T,
                       summaryFunction = defaultSummary)

