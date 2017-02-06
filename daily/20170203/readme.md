# python difflib json

```python
import difflib
import json


def _default_tostring(d, default=str):
    return json.dumps(d, indent=2, ensure_ascii=False, sort_keys=True, default=default)


def diff(d0, d1, tostring=_default_tostring, fromfile="left", tofile="right", n=3, terminator="\n"):
    """fancy diff"""
    s0 = tostring(d0).split(terminator)
    s1 = tostring(d1).split(terminator)
    return difflib.unified_diff(s0, s1, fromfile=fromfile, tofile=tofile, lineterm="", n=n)
```

# python 微妙な関数たち

もうちょっと良い方法がありそう

```python
import urllib.parse


def url_replace(uri, uri0, uri1):
    parsed = urllib.parse.urlparse(uri)
    parsed0 = urllib.parse.urlparse(uri0)
    if parsed.netloc != parsed0.netloc:
        return uri
    parsed1 = urllib.parse.urlparse(uri1)
    replaced = parsed._replace(scheme=parsed1.scheme, netloc=parsed1.netloc)
    return urllib.parse.urlunparse(replaced)


def dict_softupdate(d0, d1):
    for k, v in d1.items():
        if k not in d0:
            d0[k] = v
    return d0
```
