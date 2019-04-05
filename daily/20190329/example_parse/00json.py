import json

with open("./data/ok.json") as rf:
    d = json.load(rf)
    print(d)

try:
    with open("./data/ng.json") as rf:
        d = json.load(rf)
        print(d)
except Exception as e:
    print("!!", type(e), e)
