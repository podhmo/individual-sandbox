# marshmallow jsonschemaと本当に互換性を保つために

ChainMapで返したほうが良いのかな。

例えば、以下のようなschemaのときにjsonschema的にはdefaultではadditionalPropertiesはtrueなのだよなー。

```json
{
  "type": "object",
  "required": [
    "name",
    "age"
  ],
  "properties": {
    "name": {
      "type": "string"
    },
    "age": {
      "type": "number"
    }

  }
}
```

なので、 `{"name": "foo", "age": 10, "memo": "hai"}` の `"memo"` も消されない。
ただ、一方でmarshmallowで以下のような定義にしてしまうと `"memo"` を消してしまう。

```python
class Person(Schema):
    name = fields.String(required=True)
    age = fields.Integer(required=True)
```
