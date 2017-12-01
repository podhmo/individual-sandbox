import requests

r = requests.get("http://pod.hatenablog.com/")
print(type(r.content))
r = requests.get("http://pod.hatenablog.com/", stream=True)
print(type(r.content))
# stream=True option
