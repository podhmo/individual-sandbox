## jsonschema anyOfとallofを同時に使える

```json
{
    "type": "object",
    "allOf": [
        {"$ref": "#/definitions/foo"},
        {"$ref": "#/definitions/bar"}
    ],
    "anyOf": [
        {"$ref": "#/definitions/fooNames"},
        {"$ref": "#/definitions/barNames"}
    ],
    "definitions": {
        "foo": {
            "properties": {
                "foo": {"type": "string"}
            }
        },
        "fooNames": {
            "propertyNames": {"enum": ["foo"]}
        },
        "bar": {
            "properties": {
                "bar": {"type": "number"}
            }
        },
        "barNames": {
            "propertyNames": {"enum": ["bar"]}
        }
    }
}
```

そしてpropertyNamesは各要素のmap的な形で評価されるので `{"maxLength": 3, "minLength": 3}` みたいな指定も可能。

https://stackoverflow.com/questions/52897781/unclear-about-the-meaning-of-propertynames

## jsonschema v4からのupdateって何があったんだろう？

- http://json-schema.org/specification-links.html
- http://json-schema.org/draft-06/json-schema-release-notes.html#q-what-happened-to-all-the-discussions-around-re-using-schemas-with-additionalproperties
- https://json-schema.org/draft-07/json-schema-release-notes.html

## relative json pointer

- https://json-schema.org/draft-07/relative-json-pointer.html

## openapi (jsonschema) 似たような形式のものが複数ある場合どうすれば良いんだろう？

- [example_openapi](example_openapi)

## make

- https://www.gnu.org/software/make/manual/make.html#Text-Functions
