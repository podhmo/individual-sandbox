import requests
print(requests.get("https://httpbin.org/gzip").text)
