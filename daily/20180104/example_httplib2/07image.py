import httplib2
http = httplib2.Http()
response, body = http.request("https://httpbin.org/image/png", "GET")

with open("image.png", "wb") as wf:
    wf.write(body)
