import httplib2
http = httplib2.Http()
response, body = http.request("http://pod.hatenablog.com/", "GET")
print(response)
