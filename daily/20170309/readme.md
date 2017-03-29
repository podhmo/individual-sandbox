# elastic search

知らなかったもの(TODO: sample)

range

- https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-range-query.html

aggregations

- https://www.elastic.co/guide/en/elasticsearch/reference/current/_executing_aggregations.html
- https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket.html

# elastic search documentの検索

update_by_query with painless

- https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-update-by-query.html
- https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-update.html
- https://www.elastic.co/guide/en/elasticsearch/reference/master/modules-scripting-painless-syntax.html

全部見る

```
http ":9200/<index>/<type>/_search?q=*:*&pretty=true"
```

何か探す

```
echo '{"query": {"match": {"<field>": "<query>"}}}' | http POST ":9200/<index>/<type>/_search?q=*:*&pretty=true"
```

細かな条件

```
echo '{"query": {"bool": {"must": [{"match": {"<field>": "<query>"}}]}}' | http POST ":9200/<index>/<type>/_search?q=*:*&pretty=true"
```

# elastic search indexのmapping見る

```
http :9200/<index>
```

# elastic search index探す

```
http :9200/_cat/indices
```
