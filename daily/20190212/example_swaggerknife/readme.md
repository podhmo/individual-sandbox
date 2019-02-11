やっていること

1. [data/*.json](./data) のデータからschemaを推測
2. schema/*.json をまとめて[schema/main.json](schema/main.json)を作成
3. schema/main.jsonをたどって１つのファイルにまとめる([bundle.json](bundle.json))
4. schema定義をみてpythonのコードを生成([schema.py](schema.py))

```console
$ make
swaggerknife json2swagger data/db*.json --name db --dst schema/db.json
swaggerknife json2swagger data/flags.json --name flags --dst schema/flags.json
swaggerknife json2swagger data/api.json --name api --dst schema/api.json
jsonknife bundle --src schema/main.json --dst bundle.json
swagger-marshmallow-codegen bundle.json > schema.py
 INFO: swagger_marshmallow_codegen.codegen:write schema: write api
 INFO: swagger_marshmallow_codegen.codegen:  write field: write secret-key, field=fields.String
 INFO: swagger_marshmallow_codegen.codegen:write schema: write db
 INFO: swagger_marshmallow_codegen.codegen:  write field: write host, field=fields.String
 INFO: swagger_marshmallow_codegen.codegen:  write field: write name, field=fields.String
 INFO: swagger_marshmallow_codegen.codegen:  write field: write port, field=fields.Integer
 INFO: swagger_marshmallow_codegen.codegen:write schema: write flags
 INFO: swagger_marshmallow_codegen.codegen:  write field: write testing, field=fields.Boolean
 INFO: swagger_marshmallow_codegen.codegen:write schema: write config
 INFO: swagger_marshmallow_codegen.codegen:  write field: write api, field=fields.Nested
 INFO: swagger_marshmallow_codegen.codegen:  write field: write db, field=fields.Nested
 INFO: swagger_marshmallow_codegen.codegen:  write field: write flags, field=fields.Nested
```
