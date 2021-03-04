from selenium import webdriver
from selenium.webdriver.common.keys import Keys

# driver = webdriver.Firefox()
driver = webdriver.Chrome()
driver.get("http://www.python.org")
assert "Python" in driver.title
# elem = driver.find_element_by_name("q")
elem = driver.find_element_by_css_selector(".readmore")
print("----------------------------------------")
print(elem, elem.text, elem.tag_name, "->", elem.parent)
breakpoint()
# print(elem, elem.tag_name, "->", elem.parent, elem.parent.tag_name)
print("----------------------------------------")

elem.clear()
elem.send_keys("pycon")
elem.send_keys(Keys.RETURN)
assert "No results found." not in driver.page_source

driver.close()
