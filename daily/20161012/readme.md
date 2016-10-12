# python mock

hmm

# mongodb

何を知りたいのか

## 単純なselect

```
select * from foo;
```

```
db.foo.find().pretty()
```

## whereでinで絞り込む

```
select * from foo where x in (1, 2);
```

```
db.foo.find({x: {$in: [1,2]}})
```

## selectするフィールドを絞る

```
select name, age from foo;
```

```
db.foo.find().map(x => return [x.name, x.age])
```

## and , or の条件

```
select * from users where 15 < finished_at and finished_at < 20 and (age < 30 or type = 1)
```

```
db.users.find({ finished: { $gt: 15, $lt: 20 } , $or: [ { age: { $lt: 30 } }, { type: 1 } ]})
```

