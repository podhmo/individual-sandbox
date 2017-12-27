import httplib2
import urllib.parse as parselib

http = httplib2.Http()
data = parselib.urlencode({"name": "foo"})
headers = {'Content-type': 'application/x-www-form-urlencoded'}
response, body = http.request(f"http://localhost:44444/", method="POST", body=data, headers=headers)
print(body.decode("utf-8"))
