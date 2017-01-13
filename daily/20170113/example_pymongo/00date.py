import bson
import arrow
import pymongo

client = pymongo.MongoClient("mongodb://localhost:27017/")
db = client.me


for line in db.vs.find():
    print(line)

db.vs.insert({"time": arrow.utcnow().datetime, "v": 1})
# pymongo doesn't see tz
db.vs.update_one(
    {"_id": bson.ObjectId("587857300ccee08b20540acb")},
    {
        "$set": {"retryAt": arrow.now().replace(minutes=30, hours=9).datetime},
        "$inc": {"v": 1},
    },
)
