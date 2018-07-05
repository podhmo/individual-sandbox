## bq datasetの一覧など

```
$ bq ls --format-prettyjson
$ bq show --format=prettyjson <project id>:<data set>.<table>
```

## bq credentialsを変える方法

```console
$ CLOUDSDK_CONFIG=/path/to/config ./bin/gcloud auth login
$ CLOUDSDK_CONFIG=/path/to/config /path/to/bin/bq
```

refs

- https://qiita.com/edvakf@github/items/e0272275f2c86b4e8329

## bq schemaを出力する方法

```
$ bq show --format=prettyjson bigquery-public-data:samples.wikipedia | jq '.schema.fields
$ bq show --schema --format=prettyjson [PROJECT_ID]:[DATASET].[TABLE]
```

refs

- https://stackoverflow.com/questions/43195143/is-there-a-way-to-export-a-bigquery-tables-schema-as-json
- https://cloud.google.com/bigquery/docs/managing-table-schemas

## python csv null byte stringを除外

```
$ cat <file> | python main.py
```

こういうエラーが出る時

```console 
$ cat <file> | tr -d '\000' | python main.py
```
