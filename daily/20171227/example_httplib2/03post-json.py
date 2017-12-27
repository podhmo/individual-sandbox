import httplib2
import json

http = httplib2.Http()
data = json.dumps({"name": "foo"})
headers = {'Content-type': 'application/json'}
response, body = http.request(f"http://localhost:44444/", method="POST", body=data, headers=headers)
print(body.decode("utf-8"))
