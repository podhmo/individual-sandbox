# -*- coding:utf-8 -*-
from collections import UserDict
import pymongo
import datetime


class Post(UserDict):
    pass


client = pymongo.MongoClient("mongodb://localhost:37017/")
client.drop_database("test")
db = client.test
db.posts.create_index([("author", pymongo.ASCENDING)], unique=True)

post = Post(
    author="Mike",
    text="My first blog post",
    tags=["mongodb", "python", "pymongo"],
    data=datetime.datetime.now()
)

post_id = db.posts.insert_one(post).inserted_id
print("insert id: {}".format(post_id))

print(db.posts.find()[0])
