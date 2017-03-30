import urllib.parse as p
from lib import Detector

d = Detector()
with open("/tmp/names.txt") as rf:
    for line in rf:
        d.add(line)
print(list(d.rels.values()))


for rel in sorted(d.rels.values(), key=lambda r: r.value):
    for v in sorted(rel.bigger):
        if p.quote(rel.value) > p.quote(v.lower()):
            print("!!", rel.value, "<", v, "ord", ord(rel.value), ord(v))
