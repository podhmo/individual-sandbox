import json

d = {
    "person": "name",
    "age": 20,
}

with open("data.json", "w") as wf:
    json.dump(d, wf)

with open("data.json", "r") as rf:
    d2 = json.load(rf)

assert d == d2
