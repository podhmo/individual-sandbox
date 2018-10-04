import csv
import sys

from faker import Faker

fake = Faker()
fake.seed(1)

w = csv.DictWriter(sys.stdout, fieldnames=["name", "text"])
w.writeheader()
for i in range(5):
    w.writerow({"name": fake.name(), "text": fake.text()})
