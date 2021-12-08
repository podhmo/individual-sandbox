## null

https://github.com/guregu/null/blob/master/string.go

webAPIでとりあえずrequest,responseとdbを一致した形で手抜きでweb APIを作成したい

## sqlc

```json
{
    "version": 1,
    "packages": [
      {
        "path": "tutorial",
        "name": "tutorial",
        "engine": "postgresql",
        "schema": "schema.sql",
        "queries": "query.sql",
        "emit_json_tags": true,
        "emit_db_tags": true,
        "overrides": [{
            "go_type": {
                "import": "gopkg.in/guregu/null.v4",
                "package": "null",
                "type": "String"
            },
            "db_type": "text",
            "nullable": true
        }]
      }
    ]
  }
  ```

### null.Stringsにtypeをoverrides

null.Stringの方を使いたいなら。。

https://docs.sqlc.dev/en/stable/howto/structs.html

https://docs.sqlc.dev/en/stable/reference/config.html?highlight=overrides#type-overrides

結構指定の仕方が特殊かも。


https://github.com/kyleconroy/sqlc/issues/1274

### json tagを出力

この辺を気にしないとだめか。

https://github.com/kyleconroy/sqlc/blob/ad53cfe1b6eef57d7402ed61b9a4bebad7fe7c28/internal/config/config.go

いやこれっぽいな

https://github.com/kyleconroy/sqlc/blob/768ccb6cf2090706920282ad6fe0a397034d5a14/internal/config/v_one.go