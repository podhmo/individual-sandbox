import typing as t
import re

url = "http://example.net/foo/222/bar/1"


i = 0


def _rep(m: t.Match[str]) -> str:
    global i
    i += 1
    return f"/{{id{i}}}"


rx = re.compile(r"/\d+(?:$|/)")
print(rx.search(url))
print(rx.findall(url))
print(rx.sub(_rep, url))
