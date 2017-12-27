import httplib2

http = httplib2.Http()
response, body = http.request("http://localhost:44444/", method="GET")
print(body.decode("utf-8"))
