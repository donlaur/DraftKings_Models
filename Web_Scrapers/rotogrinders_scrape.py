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
	out_hitters = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Roto_Hitters"
	out_pitchers = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Roto_Pitchers"

	year = 2018
	months = range(4, 9)
	days = {4:30, 5:31, 6:30, 7:31, 8:4}

	select(url = url_hitters, out = out_hitters, year = year, months = months, days = days, player_type = "hitter")

def select(url, out, year, months, days, player_type):
	# Initialize Chromedriver
	driver = webdriver.Chrome()

	for month in months:
		num_days = days[month]
		for day in range(1, num_days + 1):
			try:
				# Get date of interest as a string
				t = datetime.datetime(year, month, day, 0, 0)
				t = t.strftime('%Y-%m-%d')

				# Append date string to URL, and visit that URL
				current_url = url + t
				driver.get(current_url)

				wait = WebDriverWait(driver, 100)

				# Get HTML of entire page
				wait.until(EC.element_to_be_clickable((By.XPATH, "//div[@class = 'rgt-col']")))
				old_columns = driver.find_elements_by_xpath("//div[@class = 'rgt-col']")
				old_text = "".join(map(lambda x: x.text, old_columns[20:30]))

				last_two = wait.until(EC.element_to_be_clickable((By.XPATH, "//select/option[@value = 'last-two']")))
				four_weeks = wait.until(EC.element_to_be_clickable((By.XPATH, "//select/option[@value = '4weeks']")))
				two_weeks = wait.until(EC.element_to_be_clickable((By.XPATH, "//select/option[@value = '2weeks']")))
				one_week = wait.until(EC.element_to_be_clickable((By.XPATH, "//select/option[@value = '1week']")))

				options = [last_two, four_weeks, two_weeks, one_week]

				final_content = []

				for i in range(0, len(options)):
					option = options[i]
					option.click()

					wait.until(EC.element_to_be_clickable((By.XPATH, "//div[@class = 'rgt-col']")))
					new_columns = driver.find_elements_by_xpath("//div[@class = 'rgt-col']")
					new_text = "".join(map(lambda x: x.text, new_columns[20:30]))
					counter = 0
					while old_text == new_text:
						time.sleep(2)
						new_columns = driver.find_elements_by_xpath("//div[@class = 'rgt-col']")
						new_text = "".join(map(lambda x: x.text, new_columns[20:30]))
						counter = counter + 1
						if counter == 25:
							raise Exception("Timed out!")

					if not final_content:
						columns = []
						for column in new_columns:
							columns.append([val.text.encode('utf8') for val in column.find_elements_by_xpath("./div")])
						final_content = columns
					else:
						columns = []
						for column in new_columns[20:30]:
							columns.append([val.text.encode('utf8') for val in column.find_elements_by_xpath("./div")])
						final_content = final_content + columns

					old_text = new_text

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