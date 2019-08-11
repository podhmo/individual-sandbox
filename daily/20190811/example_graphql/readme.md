## graphqlのこと

やること

- graphqlに親しむ
- graphqlのparserと親しむ

## 00 gqlgenのtodo

- graphqlと親しんだ
- https://github.com/graphql/graphql-spec これ覗いたほうが早くない？（追記)

## 01 parser

- docs https://graphql-core-next.readthedocs.io/en/latest/
- github https://github.com/graphql-python/graphql-core-next

```console
$ pip install graphql-core-next
```

parserの場所

- docs https://graphql-core-next.readthedocs.io/en/latest/usage/parser.html
- code https://github.com/graphql-python/graphql-core-next/blob/7649646cb1530a9dfdb76e53ec2313830ca94899/src/graphql/language/parser.py

dumpできないかな？

```console
$ pyinspect inspect graphql.language.ast:DocumentNode
graphql.language.ast:DocumentNode <- graphql.language.ast:Node <- builtins:object

graphql.language.ast:Node <- builtins:object
    [method] __copy__(self)
    [method] __deepcopy__(self, memo)
    [method, OVERRIDE] __eq__(self, other)
    [method, OVERRIDE] __hash__(self)
    [method, OVERRIDE] __init__(self, **kwargs)
    [class method, OVERRIDE] __init_subclass__(**kwargs)
    [method, OVERRIDE] __repr__(self)
```

### そもそもvalidateとかparseだけをしたい場合の話

https://graphql-core-next.readthedocs.io/en/latest/usage/validator.html

```python
from graphql import parse, validate


err = validate(parse(gql))
```

テキトウにparseして取り出したGraphqlSyntaxError

### graphqlのschemaを文字列では受け取れない？


