# Tutorial — PyMongo 3.4rc0 documentation
# http://api.mongodb.com/python/current/tutorial.html
import pymongo
import datetime

client = pymongo.MongoClient("mongodb://localhost:27017/")

db = client.test
client.drop_database("test")
db.posts.create_index([("author", pymongo.ASCENDING)], unique=True)

# creation
post = {
    "author": "Mike",
    "text": "My first blog post",
    "tags": ["mongodb", "python", "pymongo"],
    "date": datetime.datetime.now()
}
post_id = db.posts.insert_one(post).inserted_id
print("insert id: {}".format(post_id))


# listing collection
colls = db.collection_names(include_system_collections=False)
print("colls: {}".format(colls))

# query

print("find one: {}".format(db.posts.find_one()))
print("find one: {}".format(db.posts.find_one({"_id": post_id})))
print("find one: {}".format(db.posts.find_one({"author": "Mike"})))
print("find one: {}".format(db.posts.find_one({"author": "Eliot"})))

# bulk insert

new_posts = [
    {
        "author": "Mike2",
        "text": "My first blog post",
        "tags": ["mongodb", "python", "pymongo"],
        "date": datetime.datetime.now()
    },
    {
        "author": "Eliot",
        "title": "MongoDB is fun",
        "text": "and pretty easy too!",
        "date": datetime.datetime(2009, 11, 10, 10, 45)
    }
]
result = db.posts.insert_many(new_posts)
print(result.inserted_ids)

# all iteration
for i, post in enumerate(db.posts.find()):
    print(i, post)
# count
print("count: {}".format(db.posts.count()))
print("count: {}".format(db.posts.find({"author": "Eliot"}).count()))
# range query
d = datetime.datetime(2009, 11, 12, 12)
print("count: {}".format(db.posts.find({"date": {"$lt": d}}).sort("author").count()))


