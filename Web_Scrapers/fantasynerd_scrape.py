## Scrapes historical Daily Fantasy Nerd projections by game for every active player in the MLB, 
## and exports each day's projections as a CSV file.
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
from selenium.webdriver.common.keys import Keys
from bs4 import BeautifulSoup as BS
import csv, time, os, datetime

def main():
	# Write CSV files to this folder: change this
	output = "C:/Users/Ming/Documents/Fantasy_Models/Historical_Projections_MLB/Fantasy_Nerd"

	year = 2018
	months = range(4,9)
	days = {4:30, 5:31, 6:30, 7:31, 8:4}

	select(out = output, year = year, months = months, days = days)

def select(out, year, months, days):
	# Initialize Chromedriver
	driver = webdriver.Chrome()
	url = "https://dailyfantasynerd.com/login"
	driver.get(url)

	wait = WebDriverWait(driver, 100)
	username = wait.until(EC.element_to_be_clickable((By.ID, "input-username")))
	password = wait.until(EC.element_to_be_clickable((By.ID, "input-password")))

	username.send_keys("ming_ying@college.harvard.edu")
	password.send_keys("Jiajia98!")

	wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "btn-success"))).click()
	wait.until(EC.element_to_be_clickable((By.LINK_TEXT, "Projections"))).click()

	for month in months:
		num_days = days[month]
		for day in range(1, num_days + 1):
			try:
				t = datetime.datetime(year, month, day, 0, 0)
				t_slash = t.strftime('%m/%d/%Y')
				t = t.strftime('%Y-%m-%d')
				file_name = "{}.csv".format("nerd" + "_" + t)

				os.chdir(out)
				# Get HTML of entire page
				box = wait.until(EC.element_to_be_clickable((By.XPATH,"//div[@id = 'my-datepicker']/input")))
				box.clear()
				box.send_keys(t_slash)
				box.send_keys(Keys.ENTER)

				wait = WebDriverWait(driver, 25)
				wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "searchable")))

				game_page = driver.page_source
				soup = BS(game_page, "lxml")

				table = soup.findAll("tbody", class_ = "searchable")[0]
				rows = table.findAll("tr", class_ = "vertical-align")
				rows = map(lambda x: x.findAll("td"), rows)
				for i in range(0, len(rows)):
					rows[i] = map(lambda x: x.text.encode('utf8'), rows[i])

				with open(file_name, "ab") as file:
					writer = csv.writer(file)
					writer.writerows(row for row in rows if row)
					file.close()
			except:
				pass

if __name__ == '__main__':
    main()