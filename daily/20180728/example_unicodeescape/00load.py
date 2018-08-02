import json
import codecs

with open("./a.json") as rf:
    d = rf.read()
    print(d)

with open("./a.json", encoding="unicode-escape") as rf:
    d = rf.read()
    print(d)

with open("./a.json") as rf:
    d = json.load(rf)["full_text"]
    print(d)

with open("./b.json", "w") as wf:
    json.dump(d, wf)
    wf.write("\n")
    wf.write(d.encode("unicode-escape").decode("utf-8"))
    wf.write("\n")
    wf.write(codecs.decode(d.encode()))
    wf.write("\n")
