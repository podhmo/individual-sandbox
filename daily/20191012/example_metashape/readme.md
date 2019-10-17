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

## ふつうにパッケージに分けよう

- setup/prepare/declarative  # なんか事前に記述したいものがあれば的な
- compile
- runtime

compileという名前がおおげさな感じはしている。

## hmm

strictなどの情報をcontextとしてまとめておきたいかも？

## そういえば

これとか参考にできるかもしれない

- https://github.com/encode/typesystem


## ほしいものは

resolver部分を豊かにしたい

- 全体をwalk
- 再帰的にwalk
- 一度見たものを無視
- ある対象を見たらそれへの依存などを手軽に把握
- typing annotation対応

## 追記

テキトウに以下を実装した

- description
- required
- enum

どうやってメタデータを保持しようかな。関数を許すという形にする？
@propertyでプロパティ化してmetadataが良いかも？

あと忘れていることあったっけ？

- walking
- ref対応

ref部分ってそういえばどうやって繋げるかが明らかじゃないと困ることがあるのだよなー。
typesystemの[Reference](https://www.encode.io/typesystem/references/)あたりが参考になる気がする?

うーん。ぴったりコレではないかも。裏側の情報も持たせたいのだよなー。

## 追記

metadataをどうやってもたせるか

- 型に指定したい場合
- fieldに指定したい場合

の２つがある？

コレ自体は機能するのだけれど何かmypyでエラーになるなtyping_extensions.Annotated

- https://github.com/python/mypy/pull/7292

```console
$ python -m mypy --pretty 07annotation.py
07annotation.py:7: error: Name 'tx.Annotated' is not defined
        info: tx.Annotated[str, "info"]
              ^
07annotation.py:15: note: Revealed type is '07annotation.Person'
07annotation.py:16: note: Revealed type is 'builtins.str'
07annotation.py:17: note: Revealed type is 'builtins.str'
07annotation.py:19: error: Name 'tx.Annotated' is not defined
    foo: tx.Annotated[str, "info"] = "hai"
         ^
07annotation.py:21: note: Revealed type is 'Any'
Found 2 errors in 1 file (checked 1 source file)
```

## 追記

- refを扱いたい

  - queue的なものが欲しい。正確に言えばwalker。
  - repository不要では
  - その場で対応するものに変換する方針微妙では？
  - 単に全てが把握可能なようになっていれば十分
  - t.Optional[X]のrefが地味にめんどい
  - array?

## 追記

冷静に考えるとどういうものが欲しいんだろう？TypeInfo。context由来ではないものを返してcontext由来のものの計算をほぼ引くだけにしたい。ということなのかもしれない。

- Optional[X]とXを区別したくない
- t.Dict[str,t.Any]とdictを区別したくない(?)
- ref判定したい
- ref時のpathをlazyに決めたい
- (deps,rdepsのmapが欲しい)
- metadataは全部もっておきたい

```
- origin
- raw
- is_member
- is_optional
- is_collection
- metadata  # xxx
```

Literalって何扱い何だろう？

そして現在のrepositoryはwalkerだった説。
