# python jwt headerの情報を利用してdecode

できるはず。

# golang structのfaker

```
go install -v github.com/wawandco/fako
```

https://github.com/wawandco/fako 使えるタグの種類が書いてある。

# golang vendoringしたやつと名前が衝突してしまった場合

まだどうすれば良いか分かっていない。こういうやつ。

- [go - package's type cannot be used as the vendored package's type - Stack Overflow](http://stackoverflow.com/questions/38091816/packages-type-cannot-be-used-as-the-vendored-packages-type)

# elastic searchの情報

この記事が良い

- [知識ゼロからElasticsearchを実践で使えるようになろう！ - $shibayu36->blog;](http://blog.shibayu36.org/entry/2016/09/05/110000)

この辺も読んでおくと良い

- [Mapping | Elasticsearch Reference [5.0] | Elastic](https://www.elastic.co/guide/en/elasticsearch/reference/current/mapping.html)
- [Query and filter context | Elasticsearch Reference [5.0] | Elastic](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-filter-context.html)

以下の様な形で書くと楽？

```
count_query:
	time echo '{query: { \
		"match": { \
			"userName": "wacul" \
    }}}' \
	| http --json GET ${URL}/${INDEX_NAME}/_search | jq .hists.total
	time echo '{query: { \
		"query_string": { \
			"default_field": "_all", \
			"query": "wacul" \
    }}}' \
	| http --json GET ${URL}/${INDEX_NAME}/_search | jq .hits.total
```
