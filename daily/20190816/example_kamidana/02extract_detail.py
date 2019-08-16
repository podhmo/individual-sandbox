import sys
from kamidana.driver import Driver
from kamidana.loader import TemplateLoader
from kamidana.debug._extract import extract_detail


template = sys.argv[1]

try:
    loader = TemplateLoader([], [], [])

    driver = Driver(loader, format="raw")
    driver.run(template, None)
except Exception as e:
    for fs in extract_detail(e).framesets:
        print(len(fs))

