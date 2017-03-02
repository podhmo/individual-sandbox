# elastic searchで大文字小文字区別せずに検索したい

## readme

```bash
$ make scan-indices
# make delete-index
$ make create-index
$ make post-values
$ make get-value6 get-value7 get-value8
```

## index作成

https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-create-index.html

```
$ make create-index
```

### 単に大文字小文字を区別しないならfilterを通すだけでも

- https://www.elastic.co/guide/en/elasticsearch/reference/current/analysis-custom-analyzer.html


### ICU filter

https://www.elastic.co/guide/en/elasticsearch/plugins/current/analysis-icu-normalization-charfilter.html

## memo

https://www.elastic.co/guide/en/elasticsearch/guide/current/_indexing_employee_documents.html
