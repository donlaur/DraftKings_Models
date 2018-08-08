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
	months = range(8, 9)
	days = {4:30, 5:31, 6:30, 7:31, 8:4}

	select(year = year, months = months, days = days)

def select(year, months, days):
	# Initialize Chromedriver
	driver = webdriver.Chrome()

	url = "https://www.sabersim.com/mlb/dfs"

	driver = webdriver.Chrome()
	driver.get(url)
	time.sleep(45)

	for month in months:
		num_days = days[month]
		for day in range(1, num_days + 1):
			t = datetime.datetime(year, month, day, 0, 0)
			t = t.strftime('%m/%d/%Y')
			try:
				# What current table looks like
				wait = WebDriverWait(driver, 100)
				wait_to_load(wait, t, 4)

				wait.until(EC.element_to_be_clickable((By.XPATH, "//div[@id = 'exportProjections']/button"))).click()

				dropdown = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "flex-55")))
				dropdown.click()
				wait = WebDriverWait(dropdown, 100)
				wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "ng-scope")))
				options = dropdown.find_elements_by_class_name("ng-scope")

				if(len(options) > 1):
					for option in options[1:len(options)]:
						wait = WebDriverWait(driver, 100)
						refresh(wait, t, 4, option)

			except:
				pass

def wait_to_load(wait, t, refresh_rate):
	current_table = wait.until(EC.element_to_be_clickable((By.ID, "playerTable")))
	current_html = current_table.get_attribute("innerHTML")
	# Get HTML of entire page
	date_input = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "md-datepicker-input")))
	date_input.clear()
	date_input.send_keys(t)

	new_table = wait.until(EC.element_to_be_clickable((By.ID, "playerTable")))
	new_html = new_table.get_attribute("innerHTML")

	while(current_html == new_html):
		time.sleep(refresh_rate)
		new_table = wait.until(EC.element_to_be_clickable((By.ID, "playerTable")))
		new_html = new_table.get_attribute("innerHTML")

def refresh(wait, t, refresh_rate, option):
	current_table = wait.until(EC.element_to_be_clickable((By.ID, "playerTable")))
	current_html = current_table.get_attribute("innerHTML")
	# Get HTML of entire page
	option.click()

	new_table = wait.until(EC.element_to_be_clickable((By.ID, "playerTable")))
	new_html = new_table.get_attribute("innerHTML")
	counter = 0
	while(current_html == new_html and counter < 15):
		time.sleep(refresh_rate)
		new_table = wait.until(EC.element_to_be_clickable((By.ID, "playerTable")))
		new_html = new_table.get_attribute("innerHTML")	
		counter = counter + 1

	if counter < 15:
		wait.until(EC.element_to_be_clickable((By.XPATH, "//div[@id = 'exportProjections']/button"))).click()
	time.sleep(1)

if __name__ == '__main__':
    main()