from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import re
import csv
import sys


# Specify folder path of chrome driver
driver = webdriver.Chrome(r'C:\Users\Kenny\Desktop\NYCDSA\meetup_scrape\chromedriver.exe')

# Tech meetup page to start scrape
driver.get("https://www.meetup.com/find/tech")


# Create a CSV file and write to it the scraped data
with open('meetup_groups.csv', 'w', encoding = 'utf-8') as m:
	writer = csv.writer(m)
	
	while True
		try:	
			# Keep clicking the Show More button to get full length of page
			while True:
				try:
					wait_button = WebDriverWait(driver, 10)
					current_button = wait_button.until(EC.element_to_be_clickable((By.XPATH, '//div[@class="simple-post-result-wrap"]')))
					if current_button is not None:
						current_button.click()
						WebDriverWait(driver, 10).until(EC.staleness_of(current_button))
				except Exception:
					break

			# Parse out each URL for each meetup group on this page
			groupLinks = driver.find_elements_by_xpath('//li[@class="groupCard tileGrid-tile"]/div/a[@itemprop="url"]') # length is 100 on default page
			groupPages = [groupLink.get_attribute("href") for groupLink in groupLinks] # this gets all the tech group url pages by parseing the href value from the <a> tags
			#print(len(groupPages))
			#print(groupPages)

			# Initialize count of group pages scraped (for debugging purposes)
			group_number = 0

			for groupPage in groupPages:  # for each tech group web page...
				group_number += 1
				# Initialize empty dictionary to be filled with scraped data
				group_dict = {}
				
				# SCRAPING HOME PAGE
				driver.get(groupPage)
				try:
					title = driver.find_element_by_xpath("//a[@class='groupHomeHeader-groupNameLink']").text
				except:
					title = ""
				try:
					location = driver.find_element_by_xpath("//h1[@class='text--bold']").text
				except:
					location = ''
				try:
					about = driver.find_element_by_xpath("//div[@class = 'group-description runningText']//p").text
				except:
					about = ''
				try:
					members = driver.find_element_by_xpath("//div[@class = 'chunk']//p[@class='text--bold']/span").text
				except:
					members = ''
				try:
					photos = re.sub('[^0-9]', '', driver.find_element_by_xpath("//section[@id = 'photos']//h3[@class='text--sectionTitle text--bold']/span").text)
				except:
					photos = ''


			
				# SCRAPING MEMBERS PAGE
				driver.get(groupPage + 'members/')
				try:
					founded = driver.find_element_by_xpath("//div[@class='small margin-bottom']").text
				except:
					founded = ''
				try:
					group_reviews = driver.find_element_by_xpath("//a[span[contains(text(),'Group reviews')]]/span[@class='lastUnit align-right']").text
				except:
					group_reviews = ''
				try:
					upcoming_meetups =  driver.find_element_by_xpath("//a[span[contains(text(),'Upcoming Meetups')]]/span[@class='lastUnit align-right']").text
				except:
					upcoming_meetups = ''
				try:
					past_meetups = driver.find_element_by_xpath("//a[span[contains(text(),'Past Meetups')]]/span[@class='lastUnit align-right']").text
				except:
					past_meetups = ''
				try:
					worldwide = driver.find_element_by_xpath("//a[span[contains(text(),'Worldwide')]]/span[@class='lastUnit align-right']").text
				except:
					worldwide = ''
				try:
					leaders = re.sub('[^0-9]', '', driver.find_element_by_xpath("//a[contains(text(),'The Leadership Team ')]/span[@class='D_count']").text)
				except:
					leaders = ''
				try:
					topic_list = driver.find_elements_by_xpath("//div[@id='topic-box-2012']//div[@class='meta-topics-block small margin-bottom']//a")
					topics = [topics.text for topics in topic_list]
				except:
					topics = ''
				


				# SCRAPING DISCUSSION PAGE
				driver.get(groupPage + 'discussions/')
				try:
					discussions = re.sub('[^0-9]', '', driver.find_element_by_xpath("//h2[@class = 'text--sectionTitle text--bold padding--bottom']/span").text)
				except:
					discussions = ''



				# SAVE INTO DICTIONARY
				group_dict['title'] = title
				group_dict['location'] = location
				group_dict['about'] = about
				group_dict['members'] = members
				group_dict['photos'] = photos

				group_dict['founded'] = founded
				group_dict['group_reviews'] = group_reviews
				group_dict['upcoming_meetups'] = upcoming_meetups
				group_dict['past_meetups'] = past_meetups
				group_dict['worldwide'] = worldwide
				group_dict['leaders'] = leaders
				group_dict['topics'] = topics

				group_dict['discussions'] = discussions

				# # WRITE DICTIONARY VALUES AS A ROW IN THE CSV
				writer.writerow(group_dict.values())

				# Print for debugging purposes	
				print('*'*50)
				print(title)
				print(location)
				print(about)
				print(members)
				print(photos)
				print(founded)
				print(group_reviews)
				print(upcoming_meetups)
				print(past_meetups)
				print(worldwide)
				print(leaders)
				print(topics)
				print(discussions)
				print('group number =', group_number)
				print('*'*50)

	

		except Exception as e:
			print(e)
			driver.close()
			break