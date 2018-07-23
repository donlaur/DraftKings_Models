## Scrapes deposits by state data from FDIC website over selected period of years.
## To run this file, pip install the packages below and install the chromedriver
## application onto your computer and create a PATH variable to the chromedriver
## folder.
##
## Ming Ying, 2018.

from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait, Select
from selenium.webdriver.common.by import *
from selenium.webdriver.support import expected_conditions as EC
from bs4 import BeautifulSoup as BS
import csv, time
import urllib2 as readurl

def main():
	url = "https://rotogrinders.com/projected-stats/mlb-hitter?site=draftkings"
	select(url = url)

def select(url):
	soup = BS(readurl.urlopen(url).read())
	


	# Create Chrome Driver
	driver = webdriver.Chrome()
	# Load URL
	driver.get(url)
	# Get all 50 states
	newpage = driver.page_source
	soup = BS(newpage, "lxml")
		# Extract data by row
		data = soup.findAll('table')[3]
		data_by_row = data.find_all("tr")
		rows = []
		for row in data_by_row[6:]:
			rows.append([val.text.encode('utf8') for val in row.find_all(["td", "th"])])
		rows = [map(lambda x: ''.join(c for c in x if c not in '\n\t\xc2\xa0'), row) for row in rows]
		del rows[-1]
		if options.index(option) != 0:
			del rows[0]
		# Write table to CSV file
		filename = "deposits{}.csv".format(year)
		with open(filename, "ab") as file:
			writer = csv.writer(file)
			writer.writerows(row for row in rows if row)
			file.close()

		# Go back to previous page
		driver.find_element_by_link_text("Go Back").click()
	# Success!
	print(str(year) + " data successfully exported to CSV.")

def closepopup(driver):
	try:
		driver.find_element_by_id("decline").click()
	except:
		pass

if __name__ == '__main__':
    main()