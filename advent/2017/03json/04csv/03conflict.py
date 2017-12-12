import csv

with open("conflict.csv") as rf:
    L = list(csv.DictReader(rf))

print(L)
[{'age': '20', 'name': 'bar'}, {'age': '10', 'name': 'boo'}]
