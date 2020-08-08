## go API clientのテストとかきれいに書く方法を整理してみても良いのでは？

そもそものclient libraryを決めないと。参考になりそうな実装はなんだろう？

- https://github.com/google/go-github
- https://github.com/googleapis/google-api-go-client
- https://github.com/stripe/stripe-go
- open api generatorの生成したコード

### なぜclientを作る？

- 利用方法を明示的にしたい
- 共通部分をwrappingしたい

  - authorization
  - error handling
  - ...

### 自分で作る場合の記事

- https://qiita.com/sawadashota/items/47d8e990f27372d4c4c8
  - 2018/10
  - RoundTripper派
  - http.Clientを差し替えられる様に
  - responseを読み込んでしまう
  - status codeをチェックしちゃう
  - 元のresponseは隠す
　- (内部でrequestしている時にchannelを経由してるけれどwaitingもしてる)

- https://qiita.com/muiyama/items/fd382b1a85a7b5071840

  - httptest.Server派

- 古め

  - https://deeeet.com/writing/2016/11/01/go-api-client/
  - https://mattn.kaoriya.net/software/lang/go/20161101151118.htm

### テストの方法

- httptestなどでServerを立てる
- RountTripperでmockする
- ライブラリ単位のinterfaceでmockする
- (どこかでrequestしたAPIのresponseを使う)
- (いい感じのresponseを返すmock serverを作る)

いい感じの記事

- http://hassansin.github.io/Unit-Testing-http-client-in-Go
- https://blog.gopheracademy.com/advent-2015/symmetric-api-testing-in-go/
- https://markphelps.me/2017/09/testing-api-clients-in-go/

### utility

- https://github.com/jarcoal/httpmock
- https://github.com/dnaeon/go-vcr

### synmmetric-api-testingしたくない？

- どうやってrequest/responseのペアをもらってくるか？
- 名前をつけて管理したい(tag)
- わざわざテストコードとそれ以外を分けたくない
- 場合によってはresponseを書き換えたい
- 本物のrequest/responseをとってきたい

## hmm

こうやるとちょっと速い。でも、main00,main01,...がたくさんできてしまうかも？

```console
$ GOBIN=/tmp go install 00get/main00.go && /tmp/main00
```
