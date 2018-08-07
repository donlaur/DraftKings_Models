source("C:/Users/Ming/Documents/Fantasy_Models/MLB_Models/baseball_class.R")


# Overlaps to test
overlaps = 5:7

# Salary cap
salary.cap = 50000

# Lineups to generate per entry
num.lineups = 150

# Dates to test
year = 2018
month = 6
days = list("4" = 30, "5" = 31, "6" = 30, "7" = 31, "8" = 4)

# Paths to required folders
path.hitters.proj = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Hitters/hitter_YEAR-0MONTH-DAY.csv"
path.pitchers.proj = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Pitchers/pitcher_YEAR-0MONTH-DAY.csv"
path.players.actual = "C:/Users/Ming/Documents/Fantasy_Models/Actual_Scores_MLB/players_YEAR-0MONTH-DAY.csv"

# Run backtesting (this will take hours and hours)
backtest(overlaps, salary.cap,
         num.lineups, path.hitters.proj,
         path.pitchers.proj, path.players.actual,
         year, month, days)