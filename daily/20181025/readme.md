## graphql

- graphql
- apollo
- dataloader
- resolver

それぞれの意味とか確認しておきたいし。ドキュメントを整理しておきたい

### graphql

(relayは死んだ？)

https://github.com/graphql

- graphiql
- graphql-language-service
- libgraphqlparser

### apollo

https://github.com/apollographql

> A community building flexible open source tools for GraphQL.

community drivenのOSSのやつなのか。

- apollo-client
- apollo-server
- apollo-cli
- react-apollo
- graphql-tools

apolloはREST APIをgraphql apiとして取り扱うためのmiddlewareっぽい。
とはいえ、apollo-clientだけを使うということはありなのかも(FR)


[engineはいろいろ機能があるっぽい](https://www.apollographql.com/engine)

- caching
- query execution tracing
- error tracking

### なぞ

- Postgraphile

### 翻ってこの記事

http://techblog.heartrails.com/2018/10/api.html

rails圏の何を使っていたのだろう？(rails圏かgraphQL圏かがわかっていない）

関連リソースへの提供の部分。

- apollo-client
- apollo-cache-inmemory
- apollo-link-batch-http (たぶんこれが気にしたいもの）
- react
- react-apollo
- graphql-ruby
- graphql-batch

graphql-batchがrails側のmiddlewareでdataloader的なやつだった記憶。

https://github.com/Shopify/graphql-batch

