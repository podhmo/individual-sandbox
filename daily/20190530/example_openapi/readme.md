## openapi (jsonschema) 似たような形式のものが複数ある場合どうすれば良いんだろう？

例えばspeccyにあるopenapi3.jsonの定義でHeader関連のdefinitionsが4つある

- Header
- HeaderWithSchema
- HeaderWithContent
- HeaderWithSchemaWithExample
- HeaderWithSchemaWithExamples

ここで

- Headerは `HeaderWithSchema | HeaderWithContent`
- HeaderWithSchemaは `HeaderWithSchemaWithExample | HeaderWithSchemaWithExamples`
- HeaderWithContentは required `[content]`
- HeaderWithSchemaWithExampleは required `[schema]`
- HeaderWithSchemaWithExamplesは required `[schema, examples]`

またすべてで共通のプロパティは

- description
- required
- deprecated
- allowEmptyValue

一部にしか無いpropertyは

- content 1
- style 2
- explode 2
- allowReserved 2
- schema 2
- examples 1

## なぞ1

なんで Header と HeaderWithSchema が分かれているんだろう？

以下で良くない？

```yaml
  Header:
    oneOf:
    - $ref: '#/definitions/HeaderWithContent'
    - $ref: '#/definitions/HeaderWithSchemaWithExample'
    - $ref: '#/definitions/HeaderWithSchemaWithExamples'
```

## なぞ2

なんで HeaderWithSchemaWithExample と HeaderWithSchemaWithExamples が分かれているんだろう？

examplesをrequiredから外して、HeaderWithSchemaWithExamplesの定義を書けば良くない？

- HeaderWithSchemaWithExampleは required `[schema]`
- HeaderWithSchemaWithExamplesは required `[schema, examples]`
