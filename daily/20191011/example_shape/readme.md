## primitiveなshapeを定義してみる

- 名前をあとで決めたい
- とりあえずjsonschema (openAPIのschemas部分)を実装してみる？
- 最終的にはariadne経由でgraphQLのexampleをくらいまではやりたい

## OAS schema

- properties
- required
- (format)
- default
- ref

名前とかどうしようかな。。
runtimeかそれ以外かみたいな話があるのだよなー。

## 設計の話

ふわっと微妙な感じ

- emit()
- Resolver, FakeResolver
- Repository, FakeRepository
- (Emitter), OpenAPI

うーん。

- RepositoryとResolverが分けたいというのはどういう話なんだろう？
- meta的な設定がResolverで渡されたデータの解釈がRepositoryみたいなイメージ？
- RepositoryはResolverを内包していて良い気がする？またはその逆
- Accessor -> Repository + Resolver という感じなきもする

