## 作業

今日は何をしようか？prototypeとしての完成度を上げる感じにしていきたい。

- reflect-openapi

  - 使いみちを探る

    - api gateway?

  - イチから書いていい感じに動くか調べる

- runtime関連

  - https://golang.org/pkg/runtime/trace/
  - https://golang.org/src/runtime/symtab.go

## go reflect-openapi

- embeddedが混ざったときにうまくいかないな。。


## descriptionを取り出そうとしてみる


何か取るの面倒なときがなかったっけ？structのときだけだったかも。

```go
	// A FuncDecl node represents a function declaration.
	FuncDecl struct {
		Doc  *CommentGroup // associated documentation; or nil
		Recv *FieldList    // receiver (methods); or nil (functions)
		Name *Ident        // function/method name
		Type *FuncType     // function signature: parameters, results, and position of "func" keyword
		Body *BlockStmt    // function body; or nil for external (non-Go) function
	}
```

FuncDecl.Docが見れれば十分だっけ？関数のときはFuncDecl.Docだけで良さそう。

? methodは？

### 追記

あー、structのときだ。しかも昔に書いていた。[../20200616/readme.md](../20200616/readme.md)

```go
// Post ...
type Post struct {
	Id      int64 `db:"post_id"`
	Created int64
	Title   string `db:",size:50"`               // Column size set to 50
	Body    string `db:"article_body,size:1024"` // Set both column name and size
}

type (
	// Post2 ...
	Post2 struct {
		Id      int64 `db:"post_id"`
		Created int64
		Title   string `db:",size:50"`               // Column size set to 50
		Body    string `db:"article_body,size:1024"` // Set both column name and size
	}
)
```

