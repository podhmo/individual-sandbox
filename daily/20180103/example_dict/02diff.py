import csv
from join import innerjoin
from dictknife.guessing import guess

with open("data/users.csv") as rf:
    users = guess(list(csv.DictReader(rf)))

with open("data/users2.csv") as rf:
    users2 = guess(list(csv.DictReader(rf)))

rows = innerjoin(users, users2, left_on="id", right_on="id", suffixes=("", "2"))
for row in rows:
    r = []
    for k in row.keys():
        if k.endswith("2"):
            if hasattr(row[k], "__sub__"):
                r.append((k[:-1], row[k] - row[k[:-1]]))
            else:
                r.append((k[:-1], "{}->{}".format(row[k], row[k[:-1]])))
    print("\t".join("{k}:{v}".format(k=k, v=v) for k, v in r))
