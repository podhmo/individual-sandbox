jsonschemaだと

```yaml
name:
  type:
   - string
   - null
```

とか

```yaml
name:
  anyOf:
   - type:
     string
   - type:
     null
```

書く。

一方swaggerはtypeがstring限定でその代わりにx-nullableとかnullableとか付ける。


```yaml
name:
  type: string
  x-nullable: true
  # nullable: trueはOAS3.0
```

ただ、connexionのschemaの検証がjsonschemaに丸投げされているので上手く行かない。response validationをTrueにしたときに。
