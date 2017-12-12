import csv

with open("people.csv") as rf:
    L = list(csv.DictReader(rf))

print(L)
# [{'name': 'foo', 'age': '20'}, {'name': 'bar', 'age': '10'}]
