# setup

```
$ go get -v gopkg.in/mgo.v2/bson
```

# prepare

```
# mongodb port: 37017
$ docker-compose up
```

# run

```
$ go run 00*
Phone {ObjectIdHex("57d7c33e810149f056cccbef") Ale  2016-09-13 18:13:34.636 +0900 JST}
Results All:  [{ObjectIdHex("57d7c33e810149f056cccbef") Ale +55 53 1234 4321 2016-09-13 18:13:34.636 +0900 JST}]
Results All:  [{ObjectIdHex("57d7c33e810149f056cccbef") Ale +86 99 8888 7777 2016-09-13 18:13:34.64 +0900 JST}]
```

# result

```
$ mongo 127.0.0.1:37017/test
> show dbs
admin  (empty)
local  0.078GB
test   0.078GB
> db
test
> show collections
Cannot use 'commands' readMode, degrading to 'legacy' mode
people
system.indexes
> db.people.count()
2
> db.people.insert({"name": "x", "phone": "x", timestamp: "-"})
> db.people.count()
3
> db.people.find()
{ "_id" : ObjectId("57d7c33e810149f056cccbef"), "name" : "Ale", "phone" : "+86 99 8888 7777", "timestamp" : ISODate("2016-09-13T09:13:34.640Z") }
{ "_id" : ObjectId("57d7c33e810149f056cccbf0"), "name" : "Cla", "phone" : "+66 33 1234 5678", "timestamp" : ISODate("2016-09-13T09:13:34.636Z") }
{ "_id" : ObjectId("57d7c4e4d7164cd87a9a1e11"), "name" : "x", "phone" : "x", "timestamp" : "-" }
```
