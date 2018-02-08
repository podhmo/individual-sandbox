## goでfutureのこととかをいろいろやろうとしてみていた

何ができると嬉しいんだろう？

- 継続渡し
- チャンネル(future)
- 非同期化
- しっかり終了を待つ
- キャンセル(リトライ?)
- context

addを定義。addをadd'とpeekに分解。

```
add :: int -> (int -> (int -> unit))

add' :: int -> (int -> (int -> unit))
peek :: (int -> (int -> unit)) -> (int -> (int -> unit))
```

これがchannelになると

```
add :: int -> (int -> chan int)

add' :: int -> (int -> chan int)
peek :: (int -> chan int) -> (int -> chan int)
```

## 種類を増やす方向にも

例えばpeekは条件分岐をしていないでそのまま繋いだだけ。

- 条件に合致したら呼ばずに終了
- 条件によって分岐

この方針はあんまりよくなさそう。

例えばpeekを構成できるtraceとか。条件分岐できるbranchとか

```
trace :: (int -> unit) -> (int -> unit) -> ((int -> chan int) -> (int -> chan int))
branch :: (int -> bool) -> (int -> (int -> chan int)) -> (int -> (int -> chan int)) -> (int -> (int -> chan int))
skipif :: (int -> bool) -> (int -> (int -> chan int)) -> (int -> (int -> chan int))
```
