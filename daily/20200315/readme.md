## python graphql

- どうもgraphql-core-nextから正式にgraphql-core3に変わったっぽい

  - https://graphql-core-3.readthedocs.io/en/latest

とりあえず何がしたいんだろうか？

- ok get
- ok listing
- ok filtering
- ok order by
- pagination (limit offset, cursor)

このあたりを扱えるようになりたい。

- https://www.howtographql.com/graphql-js/8-filtering-pagination-and-sorting/

### listing

なんか変なエラーが出てしまう。

```
list indices must be integers or slices, not str', locations=[SourceLocation(line=1, column=3)], path=['people'])
```

わかっているのはsliceで文字列を渡してしまっているらしいということ。
たぶん何か記述の仕方がおかしい。

- https://medium.com/atheros/graphql-list-how-to-use-arrays-in-graphql-schema-graphql-modifiers-309d53b44c6e


わかったのはSDLベースで記述するだけだと足りなそうな感じ。

### pagination

- cursor, limit + offsetの２つがある
- https://graphql.org/learn/pagination/

hmm. argumentsの名前は慣習的なもの？

- first / after
- last / before

あー、relay cursor connectionsというものがあるのか。

- https://relay.dev/docs/en/graphql-server-specification.htm
- https://facebook.github.io/relay/graphql/connections.html
- https://qiita.com/wawoon/items/d00bd180dcac48a3068e

概ね以下のような感じ

- edgeという概念がある。
- collectionの代わりにconnectionを定義する
- PageInfoというオブジェクトを作る。

```
type PageInfo {
  hasNextPage: Boolean!
  hasPreviousPage: Boolean!
  startCursor: String
  endCursor: String
}

########################################

interface Node {
  id: ID!
}

########################################

type Ship implements Node {
  id: ID!
  name: String
}

type ShipConnection {
  edges: [ShipEdge]
  pageInfo: PageInfo!
}

type ShipEdge {
  cursor: String!
  node: Ship
}

type Query {
  ships: ShipConnection
  node(id: ID!): Node
}
```


### type

(そういえば、それぞれの概念がわかっていない気がする？)

```python
# graphql.type.schema.GraphQLSchema
schema = g.GraphQLSchema(query_type)
print(schema)

q = """\
{ people { name, age, nickname } }
"""
# graphql.langunage.ast.DocumenNode, validateが受け取るのはこれ
g.validate(schema,  g.parse(q))

# 実行はこう
print(g.graphql_sync(schema, q, Root(data)))
```

schemaからSDLを生成できないんだろうか？


### refs

- https://qiita.com/wawoon/items/e24398af912d9f3e0389
