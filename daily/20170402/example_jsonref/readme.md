```bash
$ npm install json-refs --save
$ node ./node_modules/.bin/json-refs resolve schemas/swagger.json
{
  "definitions": {
    "person": {
      "properties": {
        "name": {
          "type": "string"
        },
        "age": {
          "type": "integer",
          "minimum": 0
        }
      }
    }
  }
}
```

https://github.com/json-schema-org/json-schema-spec/issues/53#issuecomment-256171815
https://github.com/whitlockjc/json-refs
