import csv
import bson

L = [
    bson.ObjectId(),
    bson.ObjectId(),
    bson.ObjectId(),
]

with open("ids.tsv", "w") as wf:
    fields = ["analysisId"]
    writer = csv.DictWriter(wf, fields, delimiter="\t")
    writer.writeheader()
    for k in L:
        writer.writerow({"analysisId": k})
