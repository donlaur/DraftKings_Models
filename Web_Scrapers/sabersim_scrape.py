## Scrapes historical Sabersim projections by game for every active player in the MLB, and 
## export each day's projections as a CSV file. Note that access to Sabersim projection data
## requires a subscription, but that they do offer a seven-day free trial.
##
## To run this file, pip install the packages below and install the chromedriver
## application onto your computer. Then, create a PATH variable to the chromedriver
## folder (this can be done in your computer's settings).
##
## The Sabersim URL requires you to sign in to view the data. I was too lazy to implement an
## automatic sign in, so you'll have to manually sign in when the Chromedriver loads.
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
	year = 2018
	months = range(4, 9)
	days = {4:30, 5:31, 6:30, 7:31, 8:4}

	select(year = year, months = months, days = days)

def select(year, months, days):
	# Initialize Chromedriver
	driver = webdriver.Chrome()

	url = "https://www.sabersim.com/mlb/dfs"

	driver = webdriver.Chrome()
	driver.get(url)
	time.sleep(90)

	for month in months:
		num_days = days[month]
		for day in range(1, num_days + 1):
			t = datetime.datetime(year, month, day, 0, 0)
			t = t.strftime('%m/%d/%Y')
			try:
				# Get HTML of entire page
				time.sleep(30)
				wait = WebDriverWait(driver, 100)
				date_input = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "md-datepicker-input")))
				date_input.clear()
				date_input.send_keys(t)
				# Wait for page to load
				time.sleep(120)
				while driver.find_elements_by_class_name("fa-spin-add"):
					print("Sleeping")
					time.sleep(10)

				# Click on Export to CSV button
				wait.until(EC.element_to_be_clickable((By.XPATH, "//div[@id = 'exportProjections']/button"))).click()
			except:
				pass

if __name__ == '__main__':
    main()