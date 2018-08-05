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
	# Write CSV files to this folder: change this
	output = "C:/Users/Ming/Documents/Fantasy_Models/Actual_Scores_MLB"

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
			# Get date of interest as a string
			current_url = "http://rotoguru1.com/cgi-bin/byday.pl?game=dk&month={}&day={}&year={}".format(str(month), str(day), str(year))

			driver = webdriver.Chrome()
			driver.get(current_url)

			# Get HTML of entire page
			game_page = driver.page_source
			soup = BS(game_page, "lxml")

			# Get all columns on that page
			tables = soup.findAll("table")

			longest = 0
			for i in range(1,len(tables)):
				if(len(tables[i]) > len(tables[longest])):
					longest = i

			table = tables[longest]
			rows = table.findAll("tr")
			rows = map(lambda x: x.findAll("td"), rows)
			rows_to_remove = []
			for i in range(0, len(rows)):
				if len(rows[i]) < 7 or len(rows[i]) > 8:
					rows_to_remove.append(rows[i])
				elif len(rows[i]) == 8:
					rows[i] = rows[i][1:len(rows[i])]
			rows = [e for e in rows if e not in rows_to_remove]

			for i in range(0, len(rows)):
				rows[i] = map(lambda x: x.text.encode('utf8'), rows[i])

			t = datetime.datetime(year, month, day, 0, 0)
			t = t.strftime('%Y-%m-%d')
			file_name = "{}.csv".format("players" + "_" + t)

			os.chdir(out)
			with open(file_name, "ab") as file:
				writer = csv.writer(file)
				writer.writerows(row for row in rows if row)
				file.close()

if __name__ == '__main__':
    main()