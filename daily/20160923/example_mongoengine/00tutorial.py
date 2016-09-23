# http://docs.mongoengine.org/
# Tutorial — PyMongo 3.4rc0 documentation
# http://api.mongodb.com/python/current/tutorial.html

# (find-file "../../20160921/example_pymongo/00tutorial.py")

import datetime
import mongoengine as m


class Post(m.Document):
    author = m.StringField(length=32, required=True)
    title = m.StringField(length=255)
    text = m.StringField()
    tags = m.ListField(m.StringField())
    date = m.DateTimeField(default=datetime.datetime.now)

    def __str__(self):
        return str(self.to_mongo())


db = m.connect(host="mongodb://localhost:27017/test")
db.drop_database('test')

# creation
post = Post(author="Mike",
            text="My first blog post",
            tags=["mongodb", "python", "pymongo"])

post.save()
print("insert id: {}".format(post.id))

# bulk insert
r = Post.objects.insert([
    Post(author="Mike2",
         text="My first blog post",
         tags=["mongodb", "python", "pymongo"]),
    Post(author="Eliot",
         title="MongoDB is fun",
         text="and pretty easy too!",
         date=datetime.datetime(2009, 11, 10, 10, 45))
])
print("bulk inserted:{}".format(r))


# listing collection
# ? mongo show collections


# query
print("find one: {}".format(Post.objects.first()))
print("find one: {}".format(Post.objects.filter(id=post.id).first()))
print("find one: {}".format(Post.objects.filter(author="Mike").first()))
print("find one: {}".format(Post.objects.filter(author="Eliot").first()))

# all iteration
print("-")
for i, post in enumerate(Post.objects.all()):
    print(i, post)

# count
print("count: {}".format(Post.objects.count()))
print("count: {}".format(Post.objects.filter(author="Eliot").count()))


# range query
d = datetime.datetime(2009, 11, 12, 12)
print("count: {}".format(Post.objects.filter(date__lt=d).count()))
