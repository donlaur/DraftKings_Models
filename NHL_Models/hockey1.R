## Required libraries

## ------------------------------------------------------------ ##


library(ompr)
library(magrittr)
library(ROI)
library(ompr.roi)
library(rlist)
library(ROI.plugin.symphony)
library(ROI.plugin.glpk)


## Macro hockey variables: change these

## ------------------------------------------------------------ ##


# Number of lineups allowed in tournament
num.lineups = 5

# Maximum overlap of players among lineups
num.overlap = 5

# Salary cap
salary.cap = 50000

# Path to output file on personal computer
path.output = "C:/Users/Ming/Documents/FantasySports/output/stacked_symphony.csv"

# Type of solver software (either glpk or symphony)
solver = "symphony"

## Create one lineup without stacking ##

## ------------------------------------------------------------ ##


nonstacked.lineup = function(skaters, goalies, lineups,
                             num.overlap, num.skaters, num.goalies,
                             centers, wingers, defenders, num.teams,
                             skaters.teams, salary.cap) {
  
  m = MILPModel() %>%
  
    # Vector consisting of dummy variables indicating whether each skater is chosen for the lineup
    add_variable(skaters.lineup[i],
                 i = 1:num.skaters,
                 type = "binary") %>%
    
    # Vector consisting of dummy variables indicating whether each goalie is chosen for the lineup
    add_variable(goalies.lineup[i],
                 i = 1:num.goalies,
                 type = "binary") %>%
    
    # Vector consisting of dummy variables indicating whether a team is represented in the lineup
    add_variable(used.team[i],
                 i = 1:num.teams,
                 type = "binary") %>%
    
    # Eight skaters constraint
    add_constraint(sum_expr(skaters.lineup[i], i = 1:num.skaters) == 8) %>%
    
    # One goalie constraint
    add_constraint(sum_expr(goalies.lineup[i], i = 1:num.goalies) == 1) %>%
    
    # 2-3 centers
    add_constraint(sum_expr(colwise(centers[i]) * skaters.lineup[i], i = 1:num.skaters) <= 3) %>%
    add_constraint(sum_expr(colwise(centers[i]) * skaters.lineup[i], i = 1:num.skaters) >= 2) %>%
    
    # 3-4 wingers
    add_constraint(sum_expr(colwise(wingers[i]) * skaters.lineup[i], i = 1:num.skaters) <= 4) %>%
    add_constraint(sum_expr(colwise(wingers[i]) * skaters.lineup[i], i = 1:num.skaters) >= 3) %>%
    
    # 2-3 defenders
    add_constraint(sum_expr(colwise(defenders[i]) * skaters.lineup[i], i = 1:num.skaters) <= 3) %>%
    add_constraint(sum_expr(colwise(defenders[i]) * skaters.lineup[i], i = 1:num.skaters) >= 2) %>%
    
    # Budget constraint
    add_constraint(sum_expr(colwise(skaters[i, "Salary"]) * skaters.lineup[i], i = 1:num.skaters) +
                     sum_expr(colwise(goalies[i, "Salary"]) * goalies.lineup[i], i = 1:num.goalies) <= salary.cap)
  
  # Skaters must come from at least three different teams
  for(i in 1:num.teams) {
    m = add_constraint(m, used.team[i] <= sum_expr(colwise(skaters.teams[t,i]) * skaters.lineup[t], t = 1:num.skaters))
  }
  m = add_constraint(m, sum_expr(used.team[i], i = 1:num.teams) >= 3)
  
  # Overlap constraint
  for(i in 1:nrow(lineups)) {
    m = add_constraint(m, sum_expr(colwise(lineups[i,j]) * skaters.lineup[j], j = 1:num.skaters) +
                         sum_expr(colwise(lineups[i, num.skaters + j]) * goalies.lineup[j], j = 1:num.goalies) <= num.overlap)
  }
  
  # Set objective
  m = set_objective(m,
                    sum_expr(colwise(skaters[i, "Projection"]) * skaters.lineup[i], i = 1:num.skaters) +
                      sum_expr(colwise(goalies[i, "Projection"]) * goalies.lineup[i], i = 1:num.goalies),
                    sense = "max")
  
  result = solve_model(m, with_ROI(solver = solver))
  
  skaters.df = get_solution(result, skaters.lineup[i])
  goalies.df = get_solution(result, goalies.lineup[i])
  return(append(skaters.df[, "value"], goalies.df[, "value"]))
}

## Create one lineup with goalie and defensemen stacking                                
## Equivalent to the Type 4 Formulation implemented in                                  
## https://github.com/dscotthunter/Fantasy-Hockey-IP-Code/blob/master/code_for_Github.jl
## but without the power play constraint


## ------------------------------------------------------------------------------------- ##


stacked.lineup = function(skaters, goalies, lineups,
                          num.overlap, num.skaters, num.goalies,
                          centers, wingers, defenders, num.teams,
                          skaters.teams, goalies.opponents, team.lines,
                          num.lines, salary.cap) {
  
  m = MILPModel() %>%
    
    # Vector consisting of dummy variables indicating whether each skater is chosen for the lineup
    add_variable(skaters.lineup[i],
                 i = 1:num.skaters,
                 type = "binary") %>%
    
    # Vector consisting of dummy variables indicating whether each goalie is chosen for the lineup
    add_variable(goalies.lineup[i],
                 i = 1:num.goalies,
                 type = "binary") %>%
    
    # Vector consisting of dummy variables indicating whether a team is represented in the lineup
    
    add_variable(used.team[i],
                 i = 1:num.teams,
                 type = "binary") %>%
    
    # Eight skaters constraint
    add_constraint(sum_expr(skaters.lineup[i], i = 1:num.skaters) == 8) %>%
    
    # One goalie constraint
    add_constraint(sum_expr(goalies.lineup[i], i = 1:num.goalies) == 1) %>%
    
    # 2-3 centers
    add_constraint(sum_expr(colwise(centers[i]) * skaters.lineup[i], i = 1:num.skaters) <= 3) %>%
    add_constraint(sum_expr(colwise(centers[i]) * skaters.lineup[i], i = 1:num.skaters) >= 2) %>%
    
    # 3-4 wingers
    add_constraint(sum_expr(colwise(wingers[i]) * skaters.lineup[i], i = 1:num.skaters) <= 4) %>%
    add_constraint(sum_expr(colwise(wingers[i]) * skaters.lineup[i], i = 1:num.skaters) >= 3) %>%
    
    # 2-3 defenders
    add_constraint(sum_expr(colwise(defenders[i]) * skaters.lineup[i], i = 1:num.skaters) <= 3) %>%
    add_constraint(sum_expr(colwise(defenders[i]) * skaters.lineup[i], i = 1:num.skaters) >= 2) %>%
    
    # Budget constraint
    add_constraint(sum_expr(colwise(skaters[i, "Salary"]) * skaters.lineup[i], i = 1:num.skaters) +
                     sum_expr(colwise(goalies[i, "Salary"]) * goalies.lineup[i], i = 1:num.goalies) <= salary.cap)
  
  # Skaters must come from exactly three different teams, with no more than six skaters
  # from the same team
  
  for(i in 1:num.teams) {
    m = add_constraint(m, used.team[i] <= sum_expr(colwise(skaters.teams[t,i]) * skaters.lineup[t], t = 1:num.skaters))
    # m = add_constraint(m, sum_expr(colwise(skaters.teams[t,i]) * skaters.lineup[t], t = 1:num.skaters) <= 6 * used.team[i])
  }
  m = add_constraint(m, sum_expr(used.team[i], i = 1:num.teams) == 3)
  
  # # Overlap constraint
  # for(i in 1:nrow(lineups)) {
  #   m = add_constraint(m, sum_expr(colwise(lineups[i,j]) * skaters.lineup[j], j = 1:num.skaters) +
  #                        sum_expr(colwise(lineups[i, num.skaters + j]) * goalies.lineup[j], j = 1:num.goalies) <= num.overlap)
  # }
  
  # # Goalie stacking: skaters cannot be on a team that opposes the goalie's team
  # for(i in 1:num.goalies) {
  #   m = add_constraint(6 * goalies.lineup[i] + 
  #                        sum_expr(colwise(goalies.opponents[i, j]) * skaters.lineup[j], j = num.skaters) <= 6)
  # }
  # 
  # # Line stacking: at least one complete line in the lineup
  # m = add_variable(m, line.stack[i], i = 1:num.lines, type = "binary")
  # for(i in 1:num.lines) {
  #   m = add_constraint(m, 
  #                      3 * line.stack[i] <= sum_expr(colwise(team.lines[i, j]) * skaters.lineup[j], j = 1:num.skaters))
  #   m = add_constraint(m,
  #                      sum_expr(line.stack[i], i = 1:num.lines) >= 1)
  # }
  # 
  # # Partial line stacking: at least two lines with at least two people
  # m = add_variable(m, partial.line.stack[i], i:num.lines, type = "binary")
  # for(i in 1:num.lines) {
  #   m = add_constraint(m,
  #                      2 * partial.line.stack[i] <= sum_expr(colwise(team.lines[i, j]) * skaters.lineup[j], j = 1:num.skaters))
  #   m = add_constraint(m,
  #                      sum_expr(partial.line.stack[i], i = 1:num.lines) >= 2)
  # }
  
  # Set objective
  m = set_objective(m,
                    sum_expr(colwise(skaters[i, "Projection"]) * skaters.lineup[i], i = 1:num.skaters) +
                      sum_expr(colwise(goalies[i, "Projection"]) * goalies.lineup[i], i = 1:num.goalies),
                    sense = "max")
  
  result = solve_model(m, with_ROI(solver = solver))
  
  print(result)
  
  skaters.df = get_solution(result, skaters.lineup[i])
  goalies.df = get_solution(result, goalies.lineup[i])
  return(append(skaters.df[, "value"], goalies.df[, "value"]))
}

## Function to generate all lineups ##

## ------------------------------------------------------------ ##


create.lineups = function(num.lineups, num.overlap, formulation, salary.cap, path.output) {
  setwd("C:/Users/Ming/Documents/FantasySports")
  
  # Create dataframe from skaters CSV file
  skaters = read.csv("data/example_skaters.csv", stringsAsFactors = F)
  
  # Create dataframe from goalies CSV file
  goalies = read.csv("data/example_goalies.csv", stringsAsFactors = F)
  
  # Number of skaters
  num.skaters = nrow(skaters)
  
  # Number of goalies
  num.goalies = nrow(goalies)
  
  # Wingers vector
  wingers = c()
  
  # Centers vector
  centers = c()
  
  # Defenders vector
  defenders = c()
  
  # Populate wingers, centers, and defenders vectors
  
  for(i in 1:num.skaters) {
    if(skaters[i, "Position"] == "LW" | skaters[i, "Position"] == "RW" | skaters[i, "Position"] == "W") {
      wingers = append(wingers, 1)
      centers = append(centers, 0)
      defenders = append(defenders, 0)
    }
    else if(skaters[i, "Position"] == "C") {
      wingers = append(wingers, 0)
      centers = append(centers, 1)
      defenders = append(defenders, 0)
    }
    else {
      wingers = append(wingers, 0)
      centers = append(centers, 0)
      defenders = append(defenders, 1)
    }
  }
  
  # A forward is a center or winger
  forward = centers + wingers
  
  # Total number of teams
  teams = unique(skaters[,"Team"])
  num.teams = length(teams)
  
  # List where each entry is a vector that stores info on what team a player is on
  team.distribution = list()
  
  for(i in 1:num.skaters) {
    player.info = rep(0, num.teams)
    for(j in 1:num.teams) {
      if(skaters[i, "Team"] == teams[j]) {
        player.info[j] = 1
      }
    }
    team.distribution = list.append(team.distribution, player.info)
  }
  
  # Converting list into a matrix
  skaters.teams = Reduce(function(x, y) {
    rbind(x, y)
  }, team.distribution)
  
  # # Vector of opponent teams in order of goalies
  # opponents = goalies[,"Opponent"]
  # 
  # # Create matrix of opponents, where each row is the opponent profile of a goalie
  # opponents.list = list()
  # for(i in 1:length(opponents)) {
  #   for(j in 1:num.teams) {
  #     if(opponents[i] == teams[j]) {
  #       opponents.list = list.append(opponents.list, skaters.teams[,j])
  #     }
  #   }
  # }
  # goalies.opponents = Reduce(function(x, y) {
  #   rbind(x, y)
  # }, opponents.list)
  
  # # Create matrix of lines, where each row is a line (from 1 to 4) that indicates whether the 
  # # skater is on that line
  # team.lines = list()
  # 
  # for(i in 1:num.teams) {
  #   team.line = matrix(rep(0, num.skaters * 4), nrow = 4)
  #   for(j in 1:num.skaters) {
  #     if(skaters[,"Team"][j] == teams[i]) {
  #       team.line[as.numeric(skaters[,"Line"][j]), j] = 1
  #     }
  #   }
  #   team.lines = list.append(team.lines, team.line)
  # }
  # team.matrix = Reduce(function(x, y) {
  #   rbind(x, y)
  # }, team.lines)
  # 
  # num.lines = nrow(team.matrix)
  
  print(paste(paste("Initializing ROI.plugin.", solver, sep = ""), "...", sep = ""))
  print("Generating lineups (this may take a while)...")
  
  # Creating a lineup
  tracer = matrix(rep(0, num.skaters + num.goalies), nrow = 1)
  lineups = formulation(skaters, goalies, tracer, num.overlap,
                        num.skaters, num.goalies, centers, wingers,
                        defenders, num.teams, skaters.teams, 
                        goalies.opponents, team.matrix, num.lines, salary.cap)
  
  lineups = matrix(lineups, nrow = 1)
  
  for(i in 1:(num.lineups - 1)) {
    lineup = formulation(skaters, goalies, lineups, num.overlap,
                         num.skaters, num.goalies, centers, wingers,
                         defenders, num.teams, skaters.teams, 
                         goalies.opponents, team.matrix, num.lines, salary.cap)
    lineups = rbind(lineups, lineup)
  }
  
  print("Lineups successfully generated")
  
  return(lineups)
}


## Converts lineups matrix into a readable format and outputs to CSV

## ------------------------------------------------------------ ##


lineups.to.csv = function(lineups) {
  setwd("C:/Users/Ming/Documents/FantasySports")
  
  # Create dataframe from skaters CSV file
  skaters = read.csv("data/example_skaters.csv", stringsAsFactors = F)
  
  # Create dataframe from goalies CSV file
  goalies = read.csv("data/example_goalies.csv", stringsAsFactors = F)
  
  # Writing to CSV file
  for(i in 1:nrow(lineups)) {
    lineup = lineups[i,]
    indices = which(lineup == 1)
    end = length(indices)
    indices[end] = 10 - (length(lineup) - indices[end])
    first.names = append(c(skaters[indices[1:(end-1)], "First.Name"]),
                         c(goalies[indices[end], "First.Name"]))
    last.names = append(c(skaters[indices[1:(end-1)], "Last.Name"]),
                        c(goalies[indices[end], "Last.Name"]))
    full.names = paste(first.names, last.names, sep = " ")
    proj.total = sum(skaters[indices[1:end-1], "Projection"]) +
      goalies[indices[end], "Projection"]
    write.table(matrix(append(full.names, proj.total), nrow = 1),
                path.output,
                sep = ",",
                append = T,
                col.names = F,
                row.names = F)
  }
  
  print(paste("CSV successfully exported to ", path.output, sep = " "))
}


## Run lineup generator ##

## ------------------------------------------------------------ ##


df = create.lineups(num.lineups, 
                    num.overlap, 
                    stacked.lineup, 
                    salary.cap, 
                    path.output)

lineups.to.csv(df)