import httplib2
http = httplib2.Http()
response, body = http.request("https://httpbin.org/xml", "GET")
print(body)
