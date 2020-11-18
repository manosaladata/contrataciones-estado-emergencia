# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

from selenium import webdriver
from selenium.webdriver.chrome.options import Options

DRIVER_PATH = 'C:/chromedriver.exe'
driver = webdriver.Chrome(executable_path=DRIVER_PATH)
driver.get("https://apps.osce.gob.pe/perfilprov-ui/ficha/20293847038")
#driver.get('https://google.com')

#options = Options()
#options.headless = True
#options.add_argument("--window-size=1920,1200")
#driver = webdriver.Chrome(options=options, executable_path=DRIVER_PATH)
#driver.get("https://apps.osce.gob.pe/perfilprov-ui/estadistica/20293847038#inhabMJ")

#print(driver.page_source)
#players = driver.find_elements_by_xpath('/"]')
driver.find_element_by_css_selector("span.score-legend").click()
text=driver.find_elements_by_css_selector(".data-container")
lista = []
for p in range(len(text)):
    lista.append(text[p].text)
print(lista)
#driver.quit()
#print(a)