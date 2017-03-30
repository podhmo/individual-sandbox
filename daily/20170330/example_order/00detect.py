from lib import Detector


d = Detector()
cs = [
    "example.com/?nc=5",
    "example.com/…ge_sp/slidervideo_video_gmocloud_thm.jpg",
    "example.com/&form=PRFUJ1&src=IE11TR&pc=FSTE",
    "example.com/blog/",
    "example.com/blog/crossdomain-gtm/",
    "example.com/blog/crossdomain/",
    "example.com/blog/ga_sitesearch/",
    "example.com/blog/ip-address/",
    "example.com/blog/ip-addressユニバーサルカイロ",
    "example.com/function/",
]


for c in cs:
    d.add(c)
print(list(d.rels.values()))

for rel in d.rels.values():
    for v in rel.bigger:
        if rel.value.lower() > v.lower():
            print("!!", rel.value, "<", v, "ord", ord(rel.value), ord(v))
