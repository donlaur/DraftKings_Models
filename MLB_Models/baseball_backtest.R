## Backtest on previous Draftkings contests. backtest(...) returns of a list of
## historical scores, which can be compared to actual winning scores on 
## Rotogrinders' ResultsDB webpage.

source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_class.R")

# Overlaps to test
overlaps = 4:7

# Salary cap
salary.cap = 50000

# Lineups to generate per entry
num.lineups = 150

# Paths to required folders
path.hitters.proj = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Hitters/hitter_DATE.csv"
path.pitchers.proj = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Pitchers/pitcher_DATE.csv"
path.players.actual = "C:/Users/Ming/Documents/Fantasy_Models/Actual_Scores_MLB/players_DATE.csv"

# Run backtesting 
results = backtest(overlaps, salary.cap,
                   num.lineups, path.hitters.proj,
                   path.pitchers.proj, path.players.actual,
                   path.saber)


# Performance plot

# setwd(output)
# df = read.csv("backtest.csv")
# df.melt = melt(df, id = "Day")
# ggplot(data = df.melt, aes(x = Day, y = value, colour = variable)) + 
#   geom_line() + xlab("Day") + ylab("Draftkings Score")