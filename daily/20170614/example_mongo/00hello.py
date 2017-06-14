import bson
import pymongo
from collections import defaultdict
from dictknife import pp
"""
Group -* User -* Skill
"""

client = pymongo.MongoClient("mongodb://localhost:27017/")
client.drop_database("test")
db = client["test"]

groups = [
    {
        "_id": bson.ObjectId(),
        "name": "A"
    },
    {
        "_id": bson.ObjectId(),
        "name": "B"
    },
]
users = [
    {
        "_id": bson.ObjectId(),
        "group_id": groups[0]["_id"],
        "name": "foo"
    },
    {
        "_id": bson.ObjectId(),
        "group_id": groups[0]["_id"],
        "name": "bar"
    },
    {
        "_id": bson.ObjectId(),
        "group_id": groups[0]["_id"],
        "name": "baz"
    },
    {
        "_id": bson.ObjectId(),
        "group_id": groups[1]["_id"],
        "name": "boo"
    },
]
skills = [
    {
        "_id": bson.ObjectId(),
        "user_id": users[0]["_id"],
        "name": "X"
    },
    {
        "_id": bson.ObjectId(),
        "user_id": users[1]["_id"],
        "name": "X"
    },
    {
        "_id": bson.ObjectId(),
        "user_id": users[2]["_id"],
        "name": "X"
    },
    {
        "_id": bson.ObjectId(),
        "user_id": users[1]["_id"],
        "name": "Y"
    },
    {
        "_id": bson.ObjectId(),
        "user_id": users[2]["_id"],
        "name": "Y"
    },
    {
        "_id": bson.ObjectId(),
        "user_id": users[2]["_id"],
        "name": "Z"
    },
]
db.groups.insert_many(groups)
db.users.insert_many(users)
db.skills.insert_many(skills)

# join by hand

users = list(db.users.find({}))
skills = db.skills.find({"user_id": {"$in": list({u["_id"] for u in users})}})
skill_map = defaultdict(list)
for s in skills:
    skill_map[s["user_id"]].append(s)

groups = db.groups.find({"_id": {"$in": list({u["group_id"] for u in users})}})
group_map = {s["_id"]: s for s in groups}

tris = []
for u in users:
    tris.append(dict(user=u, group=group_map[u["group_id"]], skills=skill_map[u["_id"]]))

pp(tris)
