import json
d = {
    "attributes": [["sheet", "*xlsx.Sheet"]],
    "sheet": [],
    "func": "WriteKuku",
    "name": "くく"
}


for i in range(1, 10):
    row = [["constf", i * j] for j in range(1, 10)]
    d["sheet"].append(row)


print(json.dumps([d], indent=2, ensure_ascii=False))
