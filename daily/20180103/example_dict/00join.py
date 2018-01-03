import json
import csv
from join import innerjoin

with open("data/users.csv") as rf:
    users = list(csv.DictReader(rf))

with open("data/groups.csv") as rf:
    groups = list(csv.DictReader(rf))

rows = innerjoin(users, groups, left_on="group_id", right_on="id", suffixes=("", "_g"))
for row in rows:
    print(json.dumps(row, sort_keys=True))
