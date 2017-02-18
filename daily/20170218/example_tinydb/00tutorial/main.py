from tinydb import TinyDB, Query

# https://tinydb.readthedocs.io/en/latest/getting-started.html


def output(x):
    print("")
    print(x)


db = TinyDB("summaries.json")

# cleanup:
db.purge()


# insert

db.insert({"type": "apple", "count": 7})
db.insert({"type": "peach", "count": 3})

# query

output(db.all())
Fruit = Query()

output(db.search(Fruit.type == "peach"))
output(db.search(Fruit.count >= 3))

db.update({"count": 10}, Fruit.type == "apple")
output(db.search(Fruit.type == "apple"))

db.remove(Fruit.count < 5)
print(db.all())
