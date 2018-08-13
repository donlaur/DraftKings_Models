source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_class.R")


# Overlaps to test
overlaps = 5:6

# Salary cap
salary.cap = 50000

# Lineups to generate per entry
num.lineups = 150

# Paths to required folders
path.hitters.proj = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Hitters/hitter_DATE.csv"
path.pitchers.proj = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Pitchers/pitcher_DATE.csv"
path.players.actual = "C:/Users/Ming/Documents/Fantasy_Models/Actual_Scores_MLB/players_DATE.csv"

# Run backtesting (this will take hours and hours)
backtest(overlaps, salary.cap,
         num.lineups, path.hitters.proj,
         path.pitchers.proj, path.players.actual,
         path.saber)


## Performance plot

setwd("C:/Users/Ming/Documents/Fantasy_Models/output")
df = read.csv("backtest.csv")
df.melt = melt(df, id = "Day")
ggplot(data = df.melt, aes(x = Day, y = value, colour = variable)) + 
  geom_line() + xlab("Day") + ylab("Draftkings Score")