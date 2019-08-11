## todo

- https://github.com/99designs/gqlgen/tree/master/example/todo
- http://localhost:8081

```
$ go run server/server.go
```

### schema

```
schema {
  query: MyQuery
  mutation: MyMutation
}

directive @hasRole(role: Role!) on FIELD_DEFINITION
directive @user(id: ID!) on MUTATION | QUERY | FIELD
scalar Map

type MyMutation {
  createTodo(todo: TodoInput!): Todo!
  updateTodo(id: ID!, changes: Map!): Todo
}

type MyQuery {
  todo(id: ID!): Todo
  lastTodo: Todo
  todos: [Todo!]!
}

enum Role {
  ADMIN
  OWNER
}

type Todo {
  id: ID!
  text: String!
  done: Boolean!
}

input TodoInput {
  text: String!
  done: Boolean
}
```

### 00query

てきとうにqueryを投げてみる

```
{
  lastTodo{ id, text, done}
}
```

output

```
{
  "data": {
    "lastTodo": {
      "id": "4",
      "text": "Please do this or else",
      "done": false
    }
  }
}
```

curl

```
curl 'http://localhost:8081/query' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: http://localhost:8081' --data-binary '{"query":"{\n  lastTodo{id,text,done}\n}"}' --compressed
```

### 01query

```
{
  todo(id: "2") {id, text,  done}
}
```

output

```
{
  "data": {
    "todo": {
      "id": "2",
      "text": "This is the most important",
      "done": false
    }
  }
}
```

:thought_balloon: curlの情報は要らなそう。

### 02query

query

```
{
  todos{id, done}
}
```

output

```
{
  "errors": [
    {
      "message": "you dont own that",
      "path": [
        "todos",
        2,
        "done"
      ]
    }
  ],
  "data": {
    "todos": [
      {
        "id": "1",
        "done": false
      },
      {
        "id": "2",
        "done": false
      },
      null,
      {
        "id": "4",
        "done": false
      }
    ]
  }
}
```

### 03query

filterする?

https://www.howtographql.com/graphql-js/8-filtering-pagination-and-sorting/

あ、提供されていないな。。todoでIDを指定して一個取り出したやつがfilter。

### 04query

sortする？これも結局引数として提供していれば使えると言うだけ。。
enumになっていることが多いっぽい。

### mutation create

query (mutation)

```
mutation {
  createTodo(todo: {text: "hekeheke", done: false}){id}
}
```

output

```
{
  "data": {
    "createTodo": {
      "id": "5"
    }
  }
}
```

endpointは同じものなので。。

```
curl 'http://localhost:8081/query' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: http://localhost:8081' --data-binary '{"query":"mutation {\n  createTodo(todo: {text: \"hekeheke\", done: false}){id}\n}"}' --compressed
```
