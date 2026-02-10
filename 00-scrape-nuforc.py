#!/usr/bin/env python3
import csv
import time
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager
from bs4 import BeautifulSoup

BASE_URL = "https://nuforc.org/subndx/?id=all"
OUT_CSV = "nuforc_all_reports_table.csv"

def scrape_current_page(driver):
    """scrape the currently visible page."""
    time.sleep(2)  # wait for js to populate
    
    soup = BeautifulSoup(driver.page_source, "html.parser")
    table = soup.find("table", class_="wpDataTable")
    
    if not table:
        raise RuntimeError("Could not find table")
    
    tbody = table.find("tbody")
    rows = []
    
    for tr in tbody.find_all("tr"):
        tds = tr.find_all("td")
        if len(tds) < 9:
            continue
        
        row_data = [td.get_text(" ", strip=True) for td in tds]
        rows.append(row_data)
    
    return rows

def click_next_page(driver):
    """click the next page button. returns true if successful, false if no more pages."""
    try:
        # find the "next" button - adjust selector based on actual html
        next_button = driver.find_element(By.CSS_SELECTOR, ".paginate_button.next:not(.disabled)")
        next_button.click()
        time.sleep(2)  # wait for new page to load
        return True
    except:
        return False

def main():
    headers = [
        "Link",
        "Occurred",
        "City",
        "State",
        "Country",
        "Shape",
        "Summary",
        "Date Reported",
        "Media",
        "Explanation",
    ]
    
    # set up chrome options
    options = webdriver.ChromeOptions()
    options.add_argument('--headless')
    options.add_argument('--no-sandbox')
    options.add_argument('--disable-dev-shm-usage')
    options.add_argument('--disable-gpu')
    
    # use webdriver-manager to get the correct chromedriver
    driver = webdriver.Chrome(
        service=Service(ChromeDriverManager().install()),
        options=options
    )
    
    all_data = []
    NUM_PAGES = 600
    
    try:
        print(f"Navigating to: {BASE_URL}")
        driver.get(BASE_URL)
        
        # wait for table to load
        WebDriverWait(driver, 10).until(
            EC.presence_of_element_located((By.CLASS_NAME, "wpDataTable"))
        )
        
        # scrape first 5 pages
        for page_num in range(1, NUM_PAGES + 1):
            print(f"Scraping page {page_num}...")
            page_data = scrape_current_page(driver)
            print(f"Found {len(page_data)} rows on page {page_num}")
            all_data.extend(page_data)
            
            # click next if not on last page
            if page_num < NUM_PAGES:
                if not click_next_page(driver):
                    print("No more pages available")
                    break
        
    finally:
        driver.quit()
    
    # write to csv
    with open(OUT_CSV, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(headers)
        writer.writerows(all_data)
    
    print(f"Wrote {len(all_data)} total rows to {OUT_CSV}")

if __name__ == "__main__":
    main()