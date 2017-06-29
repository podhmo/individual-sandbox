## graphql

使えるtypeを整理したい

### 使えるtype整理

default types

|name|desc|
|:-:|:-:|
|Int|A signed 32‐bit integer.|
|Float|A signed double-precision floating-point value.|
|String|A UTF‐8 character sequence.|
|Boolean|true or false.|
|ID|The ID scalar type represents a unique identifier, often used to refetch an object or as the key for a cache|

新しい型を自分で定義出来たりする？(幾つかの実装ではDate型もあるらしい)

その他の型

- Interface
- Enumeration types
- Union types

Interfaceのimplementationたしかに欲しいと思ったりはした。


### schema

簡単なschemaの例を用意してみたい。こういう感じ。

```
type Query {
  me: User
}

type User {
  id: ID
  name: String
}
```


### see also

- http://graphql.org/learn/schema/
