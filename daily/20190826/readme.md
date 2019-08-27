## go functional optionsの派生系

https://gist.github.com/travisjeffery/8265ca411735f638db80e2e34bdbd3ae

- validation? https://gist.github.com/logrusorgru/0d0f4ac5d7b95f79b7884b5664658bc9
- with namespace (１つのパッケージに複数のfunctional optionsの対象がある場合 (e.g. Inputに対するInputOption, Outputに対するOutputOption)
- with variation (ある条件で一部のオプションは使えないようにしたい)
- default引数の扱い(特にzero valueに書き換えたい場合など?)

interfaceを使うバージョン

- https://frasco.io/reusable-and-type-safe-options-for-go-apis-6b51d431df5d
- https://github.com/googleapis/google-api-go-client/blob/master/option/option.go

interfaceにしておくメリット

- 型のsignatureを変えたときにも利用するがわの関数の定義を変えなくて済む？
- (ただしこれは型のaliasやnew typeを付けたときも同様)
- メソッドを定義して特定の状況にだけ有効なoptionを作れる


