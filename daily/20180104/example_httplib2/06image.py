import requests

with open("image.png", "wb") as wf:
    wf.write(requests.get("https://httpbin.org/image/png").content)
