## elastic search painlessでsort

```json
{
  "query": {
    <query>
  },
  "sort" : {
    "_script" : {
      "type" : "number",
      "script" : {
        "lang": "painless",
        "inline": "doc['targetMetrics.conversions'].value - doc['otherMetrics.conversions'].value"
      },
      "order" : "desc"
    }
  },
  "size": 500
}
```

## elastic search multi field aggregation

- https://stackoverflow.com/questions/18449703/elasticsearch-group-by-multiple-fields

aggN重

```json
{
  "size": 0,
  "aggs": {
    "agg1": {
      "terms": {
        "field": "field1"
      },
      "paggs": {
        "agg2": {
          "terms": {
            "field": "field2"
          },
          "aggs": {
            "agg3": {
              "terms": {
                "field": "field3"
              }
            }
          }
        }
      }
    }
  }
}
```

sortしたい場合はこの辺

- https://www.elastic.co/guide/en/elasticsearch/guide/current/_intrinsic_sorts.html
