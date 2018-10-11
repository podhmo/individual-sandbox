import csv
import sys

from faker import Faker

fake = Faker("ja_JP")
fake.seed(2)

w = csv.DictWriter(sys.stdout, fieldnames=["url", "uri"])
w.writeheader()
for i in range(5):
    w.writerow({"uri": fake.uri(), "url": fake.url()})
