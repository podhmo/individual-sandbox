import os.path
import hashlib
import mitmproxy
from mitmproxy.http import HTTPFlow


def normalize_url(url, i):
    return "{0:04}-{1}".format(i, hashlib.sha1(url.encode("utf-8")).hexdigest())


i = 0


def response(flow: HTTPFlow):
    global i
    if flow.response is None:
        return

    url = flow.request.pretty_url
    print("[W]", url)
    outname = os.path.join("data", normalize_url(url, i))
    i += 1
    with open(outname, "wb") as f:
        f.write(flow.response.content)
