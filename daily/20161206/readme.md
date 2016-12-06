# golang mgo mongo mgoのqueryを書く部分がuntypedなので辛い。

```go
type Person struct {
	ID bson.ObjectId `bson:"_id"`
	name string `bson:"name"`
...
}

// ここがuntyped
bson.M{'_id': personID, "name": "foo"}
```


# golang delayなど色々するやつ

