## go structscan

- x/tools/go/packages の使い方忘れている
- 意外とScopeのWriteTo便利だな

```
 			pkg.Types.Scope().WriteTo(os.Stdout, 0, true)
```

WriteTo

```
package "m/01time" scope 0xc000106230 {
.  var m/01time..inittask uint8
.  type m/01time.Person struct{Name string; Age int; CraetedAt time.Time}
}
```

### 追記

できていないのが何かがわからないな

- jsonschemaを生成
- APIを生成
- embedded
- structs
- newTypes
- factories
- multi packages
- multi symbols

## python sheetconfを作った

spreadsheetを設定ファイルとして扱うやつ

- 例えばdiscordやslackのbotの設定ファイルに
- reactionでgithubのissueを作るなどの機能を付けるときに
- dbを管理したくない
- ファイルに触れない人にも設定をいじってもらいたい
