import csv

with open("ids.tsv", "r") as rf:
    reader = csv.DictReader(rf, delimiter="\t")
    for row in reader:
        print(row)
