import httplib2
http = httplib2.Http()
response, body = http.request("https://httpbin.org/gzip", "GET")
print(response)
