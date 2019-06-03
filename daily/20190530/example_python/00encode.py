import urllib.parse as p

s = 'http://example.net;state={"x:": 10, "y": 20, "name": "hello"}'
print(p.quote(p.quote(s)))
