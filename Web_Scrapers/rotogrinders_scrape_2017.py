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
	out_hitters = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Roto_Hitters_New"
	out_pitchers = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Roto_Pitchers_New"

	year = 2017
	months = range(4, 9)
	days = {4:30, 5:31, 6:30, 7:31, 8:31, 9:30}

	select(url = url_pitchers, out = out_pitchers, year = year, months = months, days = days, player_type = "pitcher")

def select(url, out, year, months, days, player_type):
	# Initialize Chromedriver
	driver = webdriver.Chrome("C:/Users/Ming/ChromeDriver/chromedriver.exe")

	for month in months:
		num_days = days[month]
		for day in range(1, num_days + 1):
			# Get date of interest as a string
			try:
				t = datetime.datetime(year, month, day, 0, 0)
				t = t.strftime('%Y-%m-%d')

				# Append date string to URL, and visit that URL
				current_url = url + t
				driver.get(current_url)

				wait = WebDriverWait(driver, 100)

				start = 20
				end = 30
				if player_type == "pitcher":
					start = 16
					end = 34

				# Get HTML of entire page
				final_content = []

				wait.until(EC.element_to_be_clickable((By.XPATH, "//div[@class = 'rgt-col']")))
				new_columns = driver.find_elements_by_xpath("//div[@class = 'rgt-col']")

				columns = []
				for column in new_columns:
					columns.append([val.text.encode('utf8') for val in column.find_elements_by_xpath("./div")])
				final_content = columns

				rows = zip(*final_content)
				file_name = "{}.csv".format(player_type + "_" + t)
				os.chdir(out)
				with open(file_name, "ab") as file:
					writer = csv.writer(file)
					writer.writerows(row for row in rows if row)
					file.close()
			except:
				pass

if __name__ == '__main__':
    main()