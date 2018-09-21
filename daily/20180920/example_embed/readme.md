#[golang]interfaceに対するfake objectの作成に埋め込みを使う

過去の以下の記事の続き。

- [goでmockを自動生成する以外に大きなinterfaceを扱う方法を考えたりしてた](https://pod.hatenablog.com/entry/2018/01/23/214742)

この方法はたまに使われていたりする。ただリンク先の記事ではinterfaceを実装したstruct（アプリで実際に利用しているstruct）を埋め込んでいる。

これはテスト中にはinterfaceを埋め込んだほうが良いかもしれないという話。

## 例

以下のようなインターフェイスがある。このインターフェイスは`F()`と`G()`の二つのメソッドを要求している。

```go
type I interface {
	F()
	G()
}
```

先程のインターフェイスIのうち、`F()`だけに依存するコードのテストのfake objectは以下で十分。

```go
type fake struct {
	I
}

func (f *fake) F() {
	fmt.Println("f")
}
```

実際以下の様なコードはコンパイルできる。

```go
func main() {
	fake := &fake{}
	fake.F()
}
```

## 定義していなかったほうのメソッドが呼ばれた場合

先程のfake objectは`F()`の利用のために`F()`だけを定義していた。実際には`G()`の実装が要求される場合にはnil dereference panicになる。

```diff
--- 00/main.go	2018-09-21 17:50:06.000000000 +0900
+++ 01/main.go	2018-09-21 17:50:23.000000000 +0900
@@ -19,5 +19,5 @@
 
 func main() {
 	fake := &fake{}
-	fake.F()
+	fake.G()
 }
```

初見では原因や修正箇所が分かりづらいかもしれない。

```
panic: runtime error: invalid memory address or nil pointer dereference
[signal SIGSEGV: segmentation violation code=0x1 addr=0x0 pc=0x107bbef]

goroutine 1 [running]:
main.main()
        VENV/daily/20180920/example_embed/01/main.go:22 +0x1f
exit status 2
(individual-sa
```

[gist](https://gist.github.com/podhmo/a132276f7692e4ab493b8311a05af6d3)
