## jq jqで.や/が在るものにアクセスする方法

```json
{
  "paths": {
    "/api/foo": {
      "description": "foo"
    },
    "~api~bar": {
      "description": "bar"
    }
  }
}
```

以下の様にquoteする。

```
$ jq -S '.paths."/api/foo"' 00data.json > .jq.output0
$ jq -S '.paths."~api~bar"' 00data.json > .jq.output0
```

