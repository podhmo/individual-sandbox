import json
import os.path
import hashlib
import mitmproxy
from mitmproxy.http import HTTPFlow


def normalize_url(url, i):
    return "{0:04}-{1}.json".format(i, hashlib.sha1(url.encode("utf-8")).hexdigest())


i = 0


def response(flow: HTTPFlow):
    global i
    if flow.response is None:
        return

    if "json" not in flow.response.headers.get("Content-Type", ""):
        return

    url = flow.request.pretty_url
    print("[W]", url)
    outname = os.path.join("data", normalize_url(url, i))
    i += 1
    with open(outname, "w") as f:
        d = json.loads(flow.response.text)
        payload = {
            "timestamp_start": flow.response.timestamp_start,
            "timestamp_end": flow.response.timestamp_end,
            "request": {
                "headers": dict(flow.request.headers.items()),
                "url": url,
            },
            "response": {
                "headers": dict(flow.response.headers.items()),
                "status_code": flow.response.status_code,
                "body": d,
            },
        }
        json.dump(payload, f, indent=2, sort_keys=True)
