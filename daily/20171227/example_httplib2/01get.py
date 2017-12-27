import httplib2
import urllib.parse as parselib

http = httplib2.Http()
qs = parselib.urlencode({"name": "foo"})
response, body = http.request(f"http://localhost:44444/?{qs}", method="GET")
print(body.decode("utf-8"))
