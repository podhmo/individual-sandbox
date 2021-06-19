## go graphql

gqlgenは使いたくない

- https://github.com/graphql-go/graphql
- https://github.com/graphql-go/handler


## go いろいろ仲良くなる

- db, api
- pointerは辛い -> sql.NullString -> jsonは辛い -> guregu/null.String
- APIのqueryは? query builderが必要になるのか -> Masterminds/squirrel


## go sql.NullStringをJSONに渡すとか

- https://github.com/guregu/null

  - gopkg.in/guregu/null.v4/null
  - gopkg.in/guregu/null.v4/zero

- https://github.com/golang/go/issues/5901

## pagination

- graphqlのpaginationの名前を借用すれば良いのでは？

  - relay style cursorか


### REST API

- https://nordicapis.com/everything-you-need-to-know-about-api-pagination/
- https://www.moesif.com/blog/technical/api-design/REST-API-Design-Filtering-Sorting-and-Pagination/#
- https://medium.com/@ignaciochiazzo/paginating-requests-in-apis-d4883d4c1c4c
- https://jsonapi.org/format/#fetching-pagination

### relay style cursor

- https://relay.dev/graphql/connections.htm
- https://github.com/graphql/graphql-relay-js/issues/94#issuecomment-232410564
- https://blog.ebiken.dev/blog/operating-graphql-server-with-gqlgen/#pagination
- https://wawoon.dev/posts/how-to-implement-relay-cursor-connection
- https://artsy.github.io/blog/2020/01/21/graphql-relay-windowed-pagination/

### interface

- edge
- pageInfo

```go
type XXXConnection {
  pageInfo PageInfo!
  edges []Edge!
}


# page.graphql
type PageInfo {
  startCursor: String!
  endCursor: String!
  hasNextPage: Boolean!
  hasPreviousPage: Boolean!
}
interface Connection {
  pageInfo: PageInfo!
  edges: [Edge]!
}
interface Edge {
  cursor: String!
  node: Node!
}
interface Node {
  identifier: ID!
}
```

### arguments

```
# page.graphql
input PaginationInput {
  first: Int
  last: Int
  before: String
  after: String
}

# こうのほうが良い？
type ForwardPaginationInput {
  first: Int!
  after: String
}

type BackwordPaginationInput {
  last: Int!
  before: String
}


```

### 気になるところ

現時点で何ページ目かわからない


### いい感じにSQLに翻訳

```
    Start from the greedy query: SELECT * FROM table
    If the after argument is provided, add id > parsed_cursor to the WHERE clause
    If the before argument is provided, add id < parsed_cursor to the WHERE clause
    If the first argument is provided, add ORDER BY id DESC LIMIT first+1 to the query
    If the last argument is provided, add ORDER BY id ASC LIMIT last+1 to the query
    If the last argument is provided, I reverse the order of the results
    If the first argument is provided then I set hasPreviousPage: false (see spec for a description of this behavior).
    If no less than first+1 results are returned, I set hasNextPage: true, otherwise I set it to false.
    If the last argument is provided then I set hasNextPage: false (see spec for a description of this behavior).
    If no less last+1 results are returned, I set hasPreviousPage: true, otherwise I set it to false.
```

### ent

- https://entgo.io/ja/docs/tutorial-todo-gql-paginate/

## go SQL injectionを防ぐためにquery builderを使うことを強制しなくても良い

- https://github.com/google/go-safeweb

面白いhack

https://github.com/google/go-safeweb/blob/399001ed71a9412ebfa8df9aa1f9ede04bfc5d3b/safesql/safesql.go#L69

## go query builder?

APIでパラメーターを受け取りたいときに、SQLを固定できない場合がある。sql injectionとか考えたくない。

- 固定して prepared statementを使う
- query builderを利用する

### query builder

意外と良いのがない？

- https://github.com/gocraft/dbr
- https://github.com/Masterminds/squirrel
- https://github.com/elgris/sqrl
- https://github.com/doug-martin/goqu
- https://github.com/ulule/loukoum

日本の人が色々作っているがあまり流行ってはいない

- https://techblog.kayac.com/golang-orm-xo-to-sqlla

### benchmark

- https://github.com/elgris/golang-sql-builder-benchmark

## go alignment

- https://itnext.io/structure-size-optimization-in-golang-alignment-padding-more-effective-memory-layout-linters-fffdcba27c61
