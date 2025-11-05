### Extreme Weather and Mortality of Vulnerable Urban Populations:  
### An Examination of Temperature and Unclaimed Deaths in New York City 

## Scrape Hart Island Lookup Service (HILS) Website
## Authors: Simon Aytes, Selen Ozdogan
## Date: September 26, 2023

## Important Note: This script was written to scrape the original Hart Island 
## Lookup Service website. Since our data collection, the City of New York 
## has replaced this website with a new platform called Hart Island Loved 
## One Lookup. The code provided below targets the previous version of the 
## website and will need to be revised to function with the current platform. 
## We include it here for transparency and to document our original data 
## collection methodology, which may be useful for researchers seeking to 
## understand our approach or adapt it for similar purposes.

# 0 # Environment Configuration

# Import libraries
from selenium import webdriver
from selenium.webdriver import ActionChains
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.support.ui import Select
from selenium.webdriver.common.by import By
import pandas as pd
import time
from tqdm import tqdm
from time import sleep
from random import randint
from datetime import datetime, timedelta
from selenium.common.exceptions import NoSuchElementException

# Utility Functions
def check_exists_by_xpath(xpath, driver):
    try:
        driver.find_element(By.XPATH, xpath)
    except NoSuchElementException:
        return False
    return True

def get_date_obj(user_date):
    temp = user_date.split("/")
    date = datetime(int(temp[2]), int(temp[0]), int(temp[1]))
    return date

def fill_date_form(driver, date_str):
    # Fill form with date range
    dateDeathFrom = driver.find_element(By.XPATH,"//*[@id='home_form:date_death_from_input']")
    dateDeathFrom.clear()
    dateDeathFrom.send_keys(date_str)
    dateDeathTo = driver.find_element(By.XPATH,"//*[@id='home_form:date_death_to_input']")
    dateDeathTo.clear()
    dateDeathTo.send_keys(date_str)

def select_gender_option(driver, gender_index):
    gender_dropdown = driver.find_element(By.XPATH, "//*[@id='home_form:gender_input']")
    gender_dropdown = Select(gender_dropdown)
    gender_dropdown.select_by_index(gender_index)

def get_row_contents(driver, n_cols):
    row_contents = []
    for p in range(1, n_cols+1):
        # obtaining the text from each column of the table
        value = driver.find_element(By.XPATH, "//*[@id='search_result_table']/tbody/tr["+str(r)+"]/td["+str(p)+"]").text
        row_contents.append(value)
    return row_contents    

def create_webdriver():
    # Setup Chrome webdriver (!!!CHANGE TO YOUR OWN DIRECTORY!!!)
    cd_service = Service("/YOUR_DIRECTORY/chromedriver-mac-x64/chromedriver")
    options = webdriver.ChromeOptions()
    driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()))
    
    # Return the driver object
    return driver

def decrement_date(date, days_to_subtract):
    return (date - timedelta(days = days_to_subtract))

def get_date_string(date):
    return date.strftime("%m/%d/%Y")

def submit_form(driver):
    # Submit the form and load new page
    driver.find_element(By.XPATH,"//*[@id='home_form:search_lk']").click()

def get_num_columns(driver):
    return len(driver.find_elements(By.XPATH, "//*[@id='search_result_table']/tbody/tr[1]/td"))

def get_num_rows(driver):
    return 1+len(driver.find_elements(By.XPATH, "//*[@id='search_result_table']/tbody/tr"))


# 1 # Scrape Data

# Start timer
tick = datetime.now()

# Features
first_name = []
last_name = []
age = []
sex = []
death_date = []
place_of_death = []
plot_no = []
case_no = []

# Define loop variables
sleep_seconds = 2.5

# Dropdown options as dict (Format -- {Label:XPath})
gender_opt_list = {'MALE':1, 'FEMALE':2, 'UNKNOWN':3}

# Create list of dates with no records
female_no_record_dates = []
male_no_record_dates = []
unknown_no_record_dates = []

# Create list of dates with 30 or more records
thirty_or_more_record_dates = []

# Get input for from- and to- dates
from_date = get_date_obj(input("Date From (older date, MM/DD/YYYY): "))
to_date = get_date_obj(input("Date To (most-recent date, MM/DD/YYYY): "))
curr_date = to_date + timedelta(days=1) # Add one to the start date because of loop structure
date_delta = to_date - from_date

# Create the webdriver
driver = create_webdriver()

# Make the window larger
driver.maximize_window()

# Loop through each day and track with a progress bar.
for i in tqdm (range(date_delta.days+1), desc="Scraping Data...", ascii=False, ncols=75): 
    # Calculate new date range by subtracting one day
    curr_date = decrement_date(curr_date, 1)
    
    # Get the date string to be input into the box on the page
    curr_date_str = get_date_string(curr_date)
    
    # Loop through all dropdown values (MALE, FEMALE, UNKNOWN)
    for gender in gender_opt_list:
        # Try to gather the data. If any errors are thrown, see 'except' block below.
        try:
            # Establish the website's address
            url = "https://a073-hartisland-web.nyc.gov/hartisland/pages/home/home.jsf"

            # Open website
            driver.get(url)

            # Wait for 'n' seconds
            sleep(sleep_seconds)
            
            # Fill form with date range
            fill_date_form(driver, curr_date_str)
            
            # Select the gender option by clicking directly to the option's XPATH
            select_gender_option(driver, gender_opt_list[gender])

            # Select "infant" if scraping infant data (comment if scraping adult data)
            #driver.find_element(By.XPATH,"//*[@id='rBtn_infant']").click()
            
            # Submit the form and load new page
            submit_form(driver)
            sleep(sleep_seconds)

            # Obtains number of rows and columns
            rows = get_num_rows(driver)
            cols = get_num_columns(driver)

            # Check if there are more than thirty records on a given date. If so, flag it
            if rows - 1 >= 30:
                thirty_or_more_record_dates.append(curr_date_str)

            # Gather the contents of table row-by-row
            for r in range(1, rows):
                # Get the contents of the row
                row_contents = get_row_contents(driver, cols)
                
                # Append values to lists
                first_name.append(row_contents[1])
                last_name.append(row_contents[0])
                age.append(row_contents[2])
                sex.append(gender)
                death_date.append(row_contents[3])
                place_of_death.append(row_contents[4])
                plot_no.append(row_contents[5])
                case_no.append(row_contents[6])

            # Return to search
            driver.find_element(By.XPATH,"//*[@id='home_form:j_id_9b']").click()
            
        # If there is an error, it means there were no records on that date for that specified gender. Log it here and move on to next.
        except Exception as e:
            # Log the dates with no records
            if gender == "MALE":
                male_no_record_dates.append(curr_date_str)
            elif gender == "FEMALE":
                female_no_record_dates.append(curr_date_str)
            elif gender == "UNKNOWN":
                unknown_no_record_dates.append(curr_date_str)
            # Continue to next loop
            continue

# When the data is gathered, close the window and log message.
driver.quit() # Close the ChromeDriver window
print("Done scraping data!")

# Stop timer
tock = datetime.now() - tick

# 2 # Output Data to CSV

# Create output dataframe with results
data = pd.DataFrame()
data['first_name'] = first_name
data['last_name'] = last_name
data['age'] = age
data['sex'] = sex
data['death_date'] = death_date
data['place_of_death'] = place_of_death
data['plot_no'] = plot_no
data['case_no'] = case_no
# !!!CHANGE TO YOUR OWN DIRECTORY!!!
file_name = '/YOUR_DIRECTORY/data/raw_data/hils_scraped_on_' + str(datetime.now()) + '.csv'
data.to_csv(file_name, index = False)

# Log dates with thirty or more records
thirty_or_more = pd.DataFrame()
thirty_or_more['date'] = thirty_or_more_record_dates
# !!!CHANGE TO YOUR OWN DIRECTORY!!!
file_name = '/YOUR_DIRECTORY/data/raw_data/thirtyormore_' + str(datetime.now()) + '.csv'
print("Dates with 30 or more records: " + str(thirty_or_more_record_dates))

# 3 # Scrape 30 or more record days separately

# Start timer
tick = datetime.now()

# Features
first_name = []
last_name = []
age = []
sex = []
death_date = []
place_of_death = []
plot_no = []
case_no = []

# Define loop variables
sleep_seconds = 2.5

# Create a list of age range strings from 0 to 110
age_range = list(range(0, 111))

# Get input for from- and to- dates
from_date = get_date_obj(input("Date From (older date, MM/DD/YYYY): "))
to_date = get_date_obj(input("Date To (most-recent date, MM/DD/YYYY): "))
curr_date = to_date + timedelta(days=1) # Add one to the start date because of loop structure
date_delta = to_date - from_date

# Write a function to fill age form
def fill_age_form(driver, age_range):
    # Fill form with age range
    AgeForm = driver.find_element(By.XPATH,"//*[@id='home_form:age_in_years_input']")
    AgeForm.clear()
    AgeForm.send_keys(age_range)

# Create the webdriver
driver = create_webdriver()

# Make the window larger
driver.maximize_window()

# Loop through each day and track with a progress bar.
for i in tqdm (range(date_delta.days+1), desc="Scraping Data...", ascii=False, ncols=75): 
    # Calculate new date range by subtracting one day
    curr_date = decrement_date(curr_date, 1)
    
    # Get the date string to be input into the box on the page
    curr_date_str = get_date_string(curr_date)
    
    # Loop through all ages and gender
    for a in age_range:
        
        # Loop through all dropdown values (MALE, FEMALE, UNKNOWN)
        for gender in gender_opt_list:

            # Try to gather the data. If any errors are thrown, see 'except' block below.
            try:
                # Establish the website's address
                url = "https://a073-hartisland-web.nyc.gov/hartisland/pages/home/home.jsf"

                # Open website
                driver.get(url)

                # Wait for 'n' seconds
                sleep(sleep_seconds)
                
                # Fill form with date range
                fill_date_form(driver, curr_date_str)
                
                # Input the age
                fill_age_form(driver, age_range[a])

                # Select the gender option by clicking directly to the option's XPATH
                select_gender_option(driver, gender_opt_list[gender])
                
                # Submit the form and load new page
                submit_form(driver)
                sleep(sleep_seconds)

                # Obtains number of rows and columns
                rows = get_num_rows(driver)
                cols = get_num_columns(driver)

                # Check if there are more than thirty records on a given date. If so, flag it
                if rows - 1 >= 30:
                    thirty_or_more_record_dates.append(curr_date_str)

                # Gather the contents of table row-by-row
                for r in range(1, rows):
                    # Get the contents of the row
                    row_contents = get_row_contents(driver, cols)
                    
                    # Append values to lists
                    first_name.append(row_contents[1])
                    last_name.append(row_contents[0])
                    age.append(row_contents[2])
                    sex.append(gender)
                    death_date.append(row_contents[3])
                    place_of_death.append(row_contents[4])
                    plot_no.append(row_contents[5])
                    case_no.append(row_contents[6])

                # Return to search
                driver.find_element(By.XPATH,"//*[@id='home_form:j_id_9b']").click()
            
            # If there is an error, it means there were no records on that date for that specified gender. Log it here and move on to next.
            except Exception as e:
                # Log the dates with no records
                if gender == "MALE":
                    male_no_record_dates.append(curr_date_str)
                elif gender == "FEMALE":
                    female_no_record_dates.append(curr_date_str)
                elif gender == "UNKNOWN":
                    unknown_no_record_dates.append(curr_date_str)
                # Continue to next loop
                continue

# When the data is gathered, close the window and log message.
driver.quit() # Close the ChromeDriver window
print("Done scraping data!")

# Stop timer
tock = datetime.now() - tick

# Create output dataframe with results
data = pd.DataFrame()
data['first_name'] = first_name
data['last_name'] = last_name
data['age'] = age
data['sex'] = sex
data['death_date'] = death_date
data['place_of_death'] = place_of_death
data['plot_no'] = plot_no
data['case_no'] = case_no
# !!!CHANGE TO YOUR OWN DIRECTORY!!!
file_name = '/YOUR_DIRECTORY/data/raw_data/thirtyormore_scraped_on_' + str(datetime.now()) + '.csv'
data.to_csv(file_name, index = False)
