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
import csv, time, os
import urllib2 as readurl

def main():
	url = "http://www.espn.com/mlb/player/_/id/32355/nick-ahmed"
	out = "C:/Users/Ming/Documents/Fantasy_Models/MLB_data"
	select(url = url, out = out)

def select(url, out):
	driver = webdriver.Chrome()
	driver.get(url)

	# page loading wait object
	wait = WebDriverWait(driver, 100)

	wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "split-select"))).click()

	teams_menu = wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "main-items")))

	wait = WebDriverWait(teams_menu, 100)

	wait.until(EC.element_to_be_clickable((By.XPATH, "./li/a[@href = '#']")))
	teams_list = teams_menu.find_elements_by_xpath("./li/a[@href = '#']")
	team_ids = map(lambda x: x.get_attribute("id"), teams_list)

	for i in range(0, len(team_ids)):
		wait = WebDriverWait(driver, 100)
		if(i > 0):
			wait.until(EC.element_to_be_clickable((By.CLASS_NAME, "split-select"))).click()
	
		# hover over the chosen team
		wait = WebDriverWait(driver, 100)
		wait.until(EC.element_to_be_clickable((By.ID, team_ids[i]))).click()

		# open menu of all teams
		wait.until(EC.presence_of_element_located((By.CLASS_NAME, "split-level-content-list")))

		# get players menu element
		players_menu = driver.find_elements_by_class_name("split-level-content-list")

		# list of all players on a specific team
		wait = WebDriverWait(players_menu[i], 100)

		wait.until(EC.element_to_be_clickable((By.XPATH, "./li")))
		team_players = players_menu[i].find_elements_by_xpath("./li/a")

		links = []
		for x in team_players:
			link = x.get_attribute("href").split("/")
			link.insert(5, "gamelog")
			links.append("/".join(link))

		for j in range(0, len(links)):
			try:
				# go to individual player's game logs
				driver.get(links[j])

				# HTML of game logs
				game_page = driver.page_source
				soup = BS(game_page, "lxml")

				data = soup.findAll('table', class_="tablehead")[0]
				header = data.find_all("tr", class_="colhead")[0]
				subheaders = data.find_all("tr", class_=["bi"])
				data_by_row = data.find_all("tr", class_=["evenrow","oddrow"])
				data_by_row = [header] + [x for x in data_by_row if x not in subheaders]
				
				rows = []
				for row in data_by_row:
					rows.append([val.text.encode('utf8') for val in row.find_all("td")])

				player_name = links[j].split("/")[-1]

				file_name = "{}.csv".format(player_name)
				with open(os.path.join(out, file_name), "ab") as file:
					writer = csv.writer(file)
					writer.writerows(row for row in rows if row)
					file.close()

			except Exception as e: print(e)

if __name__ == '__main__':
    main()