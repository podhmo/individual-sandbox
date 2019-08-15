## ctxlogをベースにもう少し使いやすい形を考えた方が良いかも

うーん。

- contextにbindする前の状態が使いにくい気がする。
- Get()で取り出したタイミングではlctxとして束縛しなければいけないのがだるい？
- ふつうに引数なしでログ出力をしたい場合もある？

追記: まとめると

- loggerのInfo()などは可変長引数で情報を付加できる
- loggerがWithError()を持つ

### contextにbindする前の状態でもフルの機能が使いたい

１つ気になったのは、contextに付ける前の段階を使うのがやりづらいかも。

```go
log := stdctxlog.New()
lctx := ctxlog.Set(ctx, log)
```

これで`WithError()`ができない感じ。logger自体は`WithError()`を定義していないので。
そうなるとlctxが観えるというのがちょっと微妙になる。

```go
func main()
	lctx := ctxlog.Set(context.Background(), stdctxlog.New())
	if err := run(lctx); err != nil {
		lctx.WithError(err).Fatal("error")
	}
}
```

lctxという変数はコード上に見せたくないかも。ユーザーには*LoggerContextという型自体を意識させたくない。そう考えるとloggerのままでWithErrorは保持したいかもなー。

```go
func main()
	l := stdctxlog.New()
	if err := run(ctxlog.Set(context.Background(), l)); err != nil {
		l.WithError(err).Fatal("error")
	}
}
```

こういう感じのコードになっていてほしいかも。

### Getで取り出した状態が使いにくい？

どういう使いかたがされるんだろう？想定している使いかたは以下だった。

```go
ctx, log := ctxlog.Get(ctx).With("xID", xID)
// ...
if err := do(ctx); err != nil {
	log.WithError(err).Info("error is occured")
	// but not break.
}
```

直接使う所で付加的な情報を追加できないのが不便と感じていた。

```go
{
	{
		ctx := ctxlog.Get(ctx)
		ctx, log := ctx.With("xID", getXID(xxx))
		log.Info("hmm") // with xID
	}
	// ここではxIDが使われない場合とき
}
```

一箇所でしか使われないログ出力のときにいちいち変数に束縛したくない。
なので以下の様な形にする予定。つまりInfoなどの出力関数が可変長引数としてbindできるようにする。

```go
{
	{
		ctxlog.Get(ctx).Info("hmm", "xID", getXID(xxx))
	}
	// ここではxIDが使われない場合とき
}
```

現状ではerrorだけが直接実行可能。これはそもそもerrへの対応がそこだけの一度切りであることが多かったせいなので。

```go
{
	{
		ctxlog.Get(ctx).WithError(err).Info("hmm")
	}
	// ここではxIDが使われない場合とき
}
```

mapみたいなやつは結局リテラルがツライのであんまり使いたくはない感じ。

```go
{
	{
		ctxlog.Get(ctx).Info("hmm", map[string]interface{}{"xID": getXID(xxx)})
	}
	// ここではxIDが使われない場合とき
}
```

型を定義してあげれば楽かと言うとそうでもない。どうしてもprefixが邪魔でこれを防ぐためにimport方法を変えたりはしたくない。

```go
{
	{
		ctxlog.Get(ctx).Info("hmm", ctxlog.M{"xID": getXID(xxx)})
	}
	// ここではxIDが使われない場合とき
}
```

そこまで引数の偶奇を気にすることのコストは高く無いから、これは余分な気がする。
雑に警告的なログ出力を出してあげれば良いだけなきがする。
（このときcallerを１つ上の階層にしてあげると良い）

### ふつうに引数なしでログ出力をしたい場合もある？

引数なしでログ出力したい場合もある？変数として束縛したくなった時にちょっと困った。


```go
ctxlog.Get(ctx).Info("hmm")
```

常にこういう形でGetで取り出すような形で実行すれば大丈夫という話はあるかも。

```go
lctx := ctxlog.Get(ctx)
lctx.Info("hmm")
```

というのもctxlog.Get()の戻り値は*ctxlog.LoggerContextなのでctxには代入できない。
ただgoっぽさを考えると不要な操作はできなくても良いという価値観が結構強かったりする（と思っている）。そう考えるとそもそも新しい状態を付加していない(Withを使っていない）のにもかかわらず変数にbindしたいという状況がそもそもありえないと考えるべきという気もする。

そう考えると今のままで問題ないような気がする。常に以下の様なctxとloggerを返すというような形の方が嬉しいのだろうか？

```go
ctx, log := ctxlog.Get(ctx)
```

このようにするとWith()などと同じカタチになる。でも、こうなると全ての操作が１行で不可能になる。With()を使うのも一度変数にbindしてからということになる。それはそれでだるそうなので。
