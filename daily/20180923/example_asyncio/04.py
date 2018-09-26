import requests
import logging
from aioknife.synclike import Background

# https://qiita.com/icoxfog417/items/07cbf5110ca82629aca0
results = []


def store_result(response):
    results.append(response.url)


urls = [
    "http://www.google.com",
    "http://www.yahoo.com",
    "https://github.com/",
]

logging.basicConfig(level=logging.DEBUG)
with Background(concurrency=2) as q:
    for url in urls:
        q.add(requests.get, url)

    for r in q:
        print("queue execution: {0}".format(r.url))
