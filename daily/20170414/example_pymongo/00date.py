import pymongo
from datetime import date


client = pymongo.MongoClient("mongodb://localhost:27017/")


db = client.test
client.drop_database("test")


# NG
try:
    day = {"name": "foo", "today": date.today()}
    print(db.days.insert_one(day))
except Exception as e:
    print("err", e)  # err Cannot encode object: datetime.date(2017, 4, 14)


# OK
day = {"name": "foo", "today": date.today().isoformat()}
print(db.days.insert_one(day))
print(db.days.find_one({}))
# {'_id': ObjectId('58f083eb0ccee07607ebf1fb'), 'today': '2017-04-14', 'name': 'foo'}
