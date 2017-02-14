# elastic search とりあえず一覧全部返す

```
http get "http://localhost:9200/<index>/_search/?pretty=true&q*:*"
```

memoどこかでqueryの情報をまとめておきたい。

# elastic search mappingの情報見る

```
http get http://localhost:9200/<index>
```

# elastic search indexの状態を一覧で見る

```
http get http://localhost:9200/_cat/indices
```


