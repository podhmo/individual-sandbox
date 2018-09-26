import aioutil
import time
import requests

st = time.time()
g = aioutil.Group(limit=3)
with requests.session() as session:
    g.go(session.put, 'http://httpbin.org/put', data=b'data')
    g.go(session.delete, 'http://httpbin.org/delete')
    g.go(session.head, 'http://httpbin.org/get')
    g.go(session.options, 'http://httpbin.org/get')
    g.go(session.patch, 'http://httpbin.org/patch', data=b'data')

for response in g.wait():
    print(response.url, response.status_code)
print(time.time() - st)
