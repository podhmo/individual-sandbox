# mongodb aggregateで複数のkeyでgroup byをしてcountしてソートする方法

- $groupの_idがgroup byされるkey
- group byのkeyはobjectもいける
- 元のmappingは$付きで取り出せる(必要なら$projectで変換できる)
- groupingされる毎にcountしたい場合には$sumに1を渡す


```
db.values.aggregate([
  {
    $limit: 10
  },
  {
    $group: {
      _id: {stage: "$state.stage", status: "$state.status"}, c: {$sum: 1}
    }
  },
  {
    $sort: {"c": -1}
  }
]);
```
