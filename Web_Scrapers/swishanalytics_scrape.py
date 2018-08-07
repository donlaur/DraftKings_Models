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
import csv, time, os, datetime, re

def main():
	# Write CSV files to this folder: change this
	output = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Swish_Analytics"

	year = 2018
	months = range(4, 9)
	days = {4:30, 5:31, 6:30, 7:31, 8:4}

	select(out = output, year = year, months = months, days = days)

def select(out, year, months, days):
	# Initialize Chromedriver
	driver = webdriver.Chrome()

	for month in months:
		num_days = days[month]
		for day in range(1, num_days + 1):
			t = datetime.datetime(year, month, day, 0, 0)
			t = t.strftime('%Y-%m-%d')
			try:
				current_url = "https://swishanalytics.com/optimus/mlb/optimus-x?date={}".format(t)

				driver = webdriver.Chrome()
				driver.get(current_url)

				# Get HTML of entire page
				wait = WebDriverWait(driver, 100)
				wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "playerPool")))

				game_page = driver.page_source
				soup = BS(game_page, "lxml")


				# Get all columns on that page
				table = soup.findAll("tbody", class_ = "playerPool")[0]
				rows = table.findAll("tr", class_ = "ng-scope")
				rows = map(lambda x: x.findAll("td")[0:4], rows)

				for i in range(0, len(rows)):
					game = rows[i][1].findAll("small", class_ = "ng-binding")[0]
					game = game.text.encode('utf8')
					rows[i][0] = rows[i][0].text.encode('utf8')
					rows[i][2] = rows[i][2].text.encode('utf8')
					rows[i][3] = ''.join(c for c in rows[i][3].text.encode('utf8') if c not in '\n')
					player_name = rows[i][1].findAll("div", class_ = "ng-binding")[0]
					rows[i][1] = ''.join(c for c in player_name.text.encode('utf8') if c not in '\n')
					rows[i][1] = ' '.join(rows[i][1].split())
					rows[i].append(game)


				file_name = "{}.csv".format("swishprojections" + "_" + t)

				os.chdir(out)
				with open(file_name, "ab") as file:
					writer = csv.writer(file)
					writer.writerows(row for row in rows if row)
					file.close()
			except:
				file_name = "{}.csv".format("swishprojections" + "_" + t)

				os.chdir(out)
				with open(file_name, "ab") as file:
					writer = csv.writer(file)
					writer.writerows(row for row in rows if row)
					file.close()

if __name__ == '__main__':
    main()