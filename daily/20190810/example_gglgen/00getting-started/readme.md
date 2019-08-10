from https://gqlgen.com/getting-started/

skeleton

```console
$ go get -v github.com/99designs/gqlgen@latest
$ go run github.com/99designs/gqlgen init
```

```
type Todo {
  id: ID!
  text: String!
  done: Boolean!
  user: User!
}

type User {
  id: ID!
  name: String!
}

type Query {
  todos: [Todo!]!
}

input NewTodo {
  text: String!
  userId: String!
}

type Mutation {
  createTodo(input: NewTodo!): Todo!
}
```
