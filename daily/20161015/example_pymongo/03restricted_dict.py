import pymongo
from kanagata import Builder
import datetime


with Builder() as b:
    with b.define_dict("Post") as post:
        post.add_member("_id", required=False)  # for mongo
        post.add_member("author")
        post.add_member("text")
        post.add_list("tags")
        post.add_list("comments", factory_name="Comment", default=list)
        post.add_member("date", default=datetime.datetime.now)
    with b.define_dict("Comment") as comment:
        comment.add_member("name")
        comment.add_member("message")

# # hook
# # see: ./03about_extend_encoder.md
# import bson
# import collections
# bson._ENCODERS[collections.Sequence] = bson._encode_list


client = pymongo.MongoClient("mongodb://localhost:37017/")
client.drop_database("test")
db = client.test
db.posts.create_index([("author", pymongo.ASCENDING)], unique=True)


m = b.build()

post = m.Post(author="Mike", text="My first blog post", tags=["mongodb", "python"])
post["tags"].append("pymongo")
post["comments"].append({"name": "foo", "message": "congrats"})

post_id = db.posts.insert_one(post).inserted_id
print("insert id: {}".format(post_id))
