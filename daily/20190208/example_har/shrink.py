import sys
import urllib.parse as p
from collections import defaultdict
from dictknife import loading

if len(sys.argv) == 1:
    # sys.argv.append("data.har")
    sys.argv.append(None)

d = loading.loadfile(sys.argv[1], format="json")
r = defaultdict(list)
for entry in d["log"]["entries"]:
    parsed = p.urlparse(entry["request"]["url"])
    if "application/json" in entry["response"]["content"]["mimeType"].lower():
        r[parsed.netloc].append(entry)
loading.dumpfile(r)
