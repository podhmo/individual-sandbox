from urllib.request import urlopen
from urllib.parse import quote_plus

webhook_url = "http://foo.bar.jp/xxx/yyy"
print(quote_plus(webhook_url))
print("----")

# http://httpbin.org/get?webhook_url=http:%2F%2Ffoo.bar.jp%2Fxxx%2Fyyy
with urlopen(f"http://httpbin.org/get?webhook_url={webhook_url}") as res:
    print(res.status)
    print(res.read().decode("utf-8"))
