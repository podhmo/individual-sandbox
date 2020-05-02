## egoist go

- https://pod.hatenablog.com/entry/2020/04/30/224205

他に何を追加すると良いんだろうか？

- subcommands
- enums

### 気になること

- goaとか読む？
- enumsのコードを試しに書いてみる？

### もう少し先のことを考えてみる

結局の所、io.Readerをどうやってstructにmappingするかというだけの話かもしれない。

- rest API
- config validation

他のことを考えて良さそう。

- websocket
- grpc
- graphql

手元で動くコードは用意しても良さそう。

## DI

全部interfaceでやるならこういうのも使えるよなー。

- https://github.com/uber-go/fx
- [example_go/12fx/main.go](example_go/12fx/main.go)
