## Functions for various linear programming objectives.


## Type A Objective: Maximize the expected number of points of hitters and 
## pitchers simultaneously

## ------------------------------------------------------------ ##


objective.a = function(model, hitters, pitchers,
                       num.teams, num.games,
                       weight.a, weight.b, 
                       weight.c, weight.d) {
  obj.a = append(hitters[,"Projection"], pitchers[,"Projection"])
  obj.a = append(obj.a, rep(0, num.teams + num.games))
  
  model$obj = obj.a
  
  return(model)
}


## Type B Objective: Maximize the sum of the expected number points of hitters
## and pitchers in addition to a weighted sum of their individual standard 
## deviations

## ------------------------------------------------------------ ##


objective.b = function(model, hitters, pitchers,
                       num.teams, num.games,
                       weight.a, weight.b, 
                       weight.c, weight.d) {
  obj.a = append(hitters[,"Projection"], pitchers[,"Projection"])
  obj.b = append(hitters[,"Sigma"] * weight.a, pitchers[,"Sigma"] * weight.b)
  obj.a = obj.a + obj.b
  obj.a = append(obj.a, rep(0, num.teams + num.games))
  
  model$obj = obj.a
  
  return(model)
}


## Type C Objective: Maximize the sum of the expected number points of hitters
## and pitchers as a first priority. Then, maximize the sum of their standard
## deviations as a second priority

## ------------------------------------------------------------ ##


objective.c = function(model, hitters, pitchers,
                       num.teams, num.games,
                       weight.a, weight.b, 
                       weight.c, weight.d) {
  obj.a = append(hitters[,"Projection"], pitchers[,"Projection"])
  obj.a = append(obj.a, rep(0, num.teams + num.games))
  
  obj.b = append(hitters[,"Sigma"], pitchers[,"Sigma"])
  obj.b = append(obj.b, rep(0, num.teams + num.games))
  
  Set            = list(obj.a, obj.b)
  SetObjPriority = c(2,1)
  SetObjWeight   = c(1.0, weight.c)
  
  model$multiobj  = list()
  for (m in 1:2) {
    model$multiobj[[m]]          = list()
    model$multiobj[[m]]$objn     = Set[[m]]
    model$multiobj[[m]]$priority = SetObjPriority[m]
    model$multiobj[[m]]$weight   = SetObjWeight[m]
    model$multiobj[[m]]$abstol   = m
    model$multiobj[[m]]$reltol   = weight.d
    model$multiobj[[m]]$con      = 0.0
  }
  
  return(model)
}


## Type D Objective: Maximize the sum of the expected number points of pitchers
## as a first priority. Then, maximize the sum of the expected number of points of 
## hitters as a second priority.

## ------------------------------------------------------------ ##


objective.d = function(model, hitters, pitchers,
                       num.teams, num.games,
                       weight.a, weight.b, 
                       weight.c, weight.d) {
  obj.a = append(rep(0, nrow(hitters)), pitchers[,"Projection"])
  obj.a = append(obj.a, rep(0, num.teams + num.games))
  
  obj.b = append(hitters[,"Projection"], rep(0, nrow(pitchers)))
  obj.b = append(obj.b, rep(0, num.teams + num.games))
  
  Set            = list(obj.a, obj.b)
  SetObjPriority = c(2,1)
  SetObjWeight   = c(1.0, weight.c)
  
  model$multiobj = list()
  for (m in 1:2) {
    model$multiobj[[m]]          = list()
    model$multiobj[[m]]$objn     = Set[[m]]
    model$multiobj[[m]]$priority = SetObjPriority[m]
    model$multiobj[[m]]$weight   = SetObjWeight[m]
    model$multiobj[[m]]$abstol   = m
    model$multiobj[[m]]$reltol   = weight.d
    model$multiobj[[m]]$con      = 0.0
  }
  
  return(model)
}


## Type E Objective: Maximize the sum of the expected number of points of pitchers as a 
## first priority. Maximize the sum of the expected number of points of hitters as a second
## priority. Maximize the sum of their standard deviations as a third priority.

## ------------------------------------------------------------ ##


objective.e = function(model, hitters, pitchers,
                       num.teams, num.games,
                       weight.a, weight.b, 
                       weight.c, weight.d,
                       weight.e) {
  obj.a = append(rep(0, nrow(hitters)), pitchers[,"Projection"])
  obj.a = append(obj.a, rep(0, num.teams + num.games))
  
  obj.b = append(hitters[,"Projection"], rep(0, nrow(pitchers)))
  obj.b = append(obj.b, rep(0, num.teams + num.games))
  
  obj.c = append(hitters[,"Sigma"], pitchers[,"Sigma"])
  obj.c = append(obj.c, rep(0, num.teams + num.games))
  
  Set            = list(obj.a, obj.b, obj.c)
  SetObjPriority = c(3,2,1)
  SetObjWeight   = c(1.0, weight.c, weight.e)
  
  model$multiobj  = list()
  for (m in 1:3) {
    model$multiobj[[m]]          = list()
    model$multiobj[[m]]$objn     = Set[[m]]
    model$multiobj[[m]]$priority = SetObjPriority[m]
    model$multiobj[[m]]$weight   = SetObjWeight[m]
    model$multiobj[[m]]$abstol   = m
    model$multiobj[[m]]$reltol   = weight.d
    model$multiobj[[m]]$con      = 0.0
  }
  
  return(model)
}


## Type F Objective: Maximize the sum of the expected number of points of pitchers and
## their weighted standard deviations as a first priority. Then, maximize the sum of the 
## expected number of points of hitters and their weighted standard deviations as a 
## second priority.

## ------------------------------------------------------------ ##


objective.f = function(model, hitters, pitchers,
                       num.teams, num.games,
                       weight.a, weight.b, 
                       weight.c, weight.d) {
  obj.a          = append(rep(0, nrow(hitters)), pitchers[,"Projection"])
  pitchers.sigma = append(rep(0, nrow(hitters)), pitchers[,"Sigma"] * weight.a)
  obj.a          = obj.a + pitchers.sigma
  obj.a          = append(obj.a, rep(0, num.teams + num.games))
  
  obj.b          = append(hitters[,"Projection"], rep(0, nrow(pitchers)))
  pitchers.sigma = append(hitters[,"Sigma"] * weight.b, rep(0, nrow(pitchers)))
  obj.b          = obj.b + pitchers.sigma
  obj.b          = append(obj.b, rep(0, num.teams + num.games))
  
  Set            = list(obj.a, obj.b)
  SetObjPriority = c(2,1)
  SetObjWeight   = c(1.0, weight.c)
  
  model$multiobj  = list()
  for (m in 1:2) {
    model$multiobj[[m]]          = list()
    model$multiobj[[m]]$objn     = Set[[m]]
    model$multiobj[[m]]$priority = SetObjPriority[m]
    model$multiobj[[m]]$weight   = SetObjWeight[m]
    model$multiobj[[m]]$abstol   = m
    model$multiobj[[m]]$reltol   = weight.d
    model$multiobj[[m]]$con      = 0.0
  }
  
  return(model)
}


## Type G Objective: Penalize high variances among expensive players by taking the reciprocal
## of players' standard deviations, multiplying by a weight, and adding to their respective projections. 


## ------------------------------------------------------------ ##


objective.g = function(model, hitters, pitchers,
                       num.teams, num.games,
                       weight.a, weight.b, 
                       weight.c, weight.d) {
  obj.a         = append(hitters[,"Projection"], pitchers[,"Projection"])
  obj.b         = rep(0, nrow(hitters) + nrow(pitchers))
  
  hitters.base  = min(hitters[,"Salary"])
  pitchers.base = min(pitchers[,"Salary"])
  
  for(i in 1:nrow(hitters)) {
    obj.b[i]                 = hitters.base * weight.a * hitters[i, "Sigma"] / hitters[i, "Salary"]
  }
  
  for(j in 1:nrow(pitchers)) {
    obj.b[nrow(hitters) + j] = pitchers.base * weight.b * pitchers[j, "Sigma"] / pitchers[j, "Salary"]
  }
  
  obj.a     = obj.a + obj.b
  obj.a     = append(obj.a, rep(0, num.teams + num.games))
  
  model$obj = obj.a
  
  return(model)
}