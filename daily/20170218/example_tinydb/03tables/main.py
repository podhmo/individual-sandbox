from tinydb import TinyDB, Query


db = TinyDB("db.json")

people = db.table("people")
groups = db.table("groups")

people.purge()
groups.purge()


people.insert_multiple([{"name": "foo"}, {"name": "bar"}, {"name": "boo"}])
groups.insert_multiple([{"name": "A"}, {"name": "B"}, {"name": "C"}])

groups.update({"members": [p.eid for p in people.all()]}, Query().name == "A")
groups.update({"members": [p.eid for p in people.all()[2:]]}, Query().name == "B")
print(groups.all())
