## Scrapes historical Rotogrinders projections by game for every active player in the MLB, and 
## export each day's projections as a CSV file
##
## To run this file, pip install the packages below and install the chromedriver
## application onto your computer. Then, create a PATH variable to the chromedriver
## folder (this can be done in your computer's settings).
##
## Ming Ying, 2018.

from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait, Select
from selenium.webdriver.common.by import *
from selenium.webdriver.support import expected_conditions as EC
from bs4 import BeautifulSoup as BS
import csv, time, os, datetime

def main():
	# Start scraping from here (any player's page will do)
	url_pitchers = "https://rotogrinders.com/projected-stats/mlb-pitcher?site=draftkings&date="
	url_hitters = "https://rotogrinders.com/projected-stats/mlb-hitter?site=draftkings&date="
	# Write CSV files to this folder: change this
	out_hitters = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections/Hitters"
	out_pitchers = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections/Pitchers"

	year = 2018
	months = range(4, 9)
	days = {4:30, 5:31, 6:30, 7:31, 8:4}

	select(url = url_pitchers, out = out_pitchers, year = year, months = months, days = days, player_type = "pitcher")
	select(url = url_hitters, out = out_hitters, year = year, months = months, days = days, player_type = "hitter")

def select(url, out, year, months, days, player_type):
	# Initialize Chromedriver
	driver = webdriver.Chrome()

	for month in months:
		num_days = days[month]
		for day in range(1, num_days + 1):
			# Get date of interest as a string
			t = datetime.datetime(year, month, day, 0, 0)
			t = t.strftime('%Y-%m-%d')

			# Append date string to URL, and visit that URL
			current_url = url + t
			driver.get(current_url)

			# Get HTML of entire page
			game_page = driver.page_source
			soup = BS(game_page, "lxml")

			# Get all columns on that page
			columns = soup.findAll("div", class_ = "rgt-col")
			columns.content = map(lambda x: x.findAll("div"), columns)

			for i in range(0, len(columns.content)):
				columns.content[i] = map(lambda x: x.text.encode('utf8'), columns.content[i])

			rows = zip(*columns.content)

			file_name = "{}.csv".format(player_type + t)

			with open(os.path.join(out, file_name), "ab") as file:
				writer = csv.writer(file)
				writer.writerows(row for row in rows if row)
				file.close()

if __name__ == '__main__':
    main()