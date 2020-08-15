## go

- encoding/json
- go test

## go encoding/json

なんとなくボーッと覗いている。

- そういえば `Unwrap()`対応していないな。
- そういえば どれくらいコストが掛かるんだろう。 `MarshalJSON()`とか`UnmarshalJSON()`
- 元の実装をどうしているかとかも気になる。

### 考えてみれば

この辺使ったことがなかったな。

- `MarshalText()`
- `UnmarshalText()`


## go bench

考えてみれば`go test`以外でbenchしたことがないかもしれない。

- https://qiita.com/marnie_ms4/items/7014563083ca1d824905#%E3%82%88%E3%81%8F%E4%BD%BF%E3%81%86testingb%E3%81%AEmethod

実際のところ `MarshalJSON()` などはどれほど遅くなっているんだろう？

## go custom marshal/unmarshal

- https://github.com/golang/go/issues/34701

結局proposalとか見ると既存のものにメソッドで追加する方針は一部のものしか受け入れられなそう。
この辺の話につながっていく。

- https://philpearl.github.io/post/aintnecessarilyslow/
- https://philpearl.github.io/post/json_iter/

そういえば、この２つの違いとか全然把握していない。

- https://github.com/modern-go/reflect2
- https://github.com/goccy/go-reflect
