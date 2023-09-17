# sqlcで関数がいっぱいできる問題

booktestをベースに考えてみる

- https://github.com/sqlc-dev/sqlc/tree/main/examples/booktest/postgresql


booktestパッケージはコピペして持ってきたもの。入力となるSQLが増えると以下あたりが増えていく。

- Queries.CreateBook(), CreateBookParams
- Queries.UpdateBook(), UpdateBOokParams
- Queries.UpdateBook(), UpdateBookISBNParams

```console
$ go doc ./booktest/
package booktest // import "m/booktest"

type Author struct{ ... }
type Book struct{ ... }
type BookType string
    const BookTypeFICTION BookType = "FICTION" ...
type BooksByTagsRow struct{ ... }
type BooksByTitleYearParams struct{ ... }
type CreateBookParams struct{ ... }
type DBTX interface{ ... }
type NullBookType struct{ ... }
type Queries struct{ ... }
    func New(db DBTX) *Queries
type UpdateBookISBNParams struct{ ... }
type UpdateBookParams struct{ ... }

$ go doc ./booktest.Queries
package booktest // import "m/booktest"

type Queries struct {
        // Has unexported fields.
}

func New(db DBTX) *Queries
func (q *Queries) BooksByTags(ctx context.Context, dollar_1 []string) ([]BooksByTagsRow, error)
func (q *Queries) BooksByTitleYear(ctx context.Context, arg BooksByTitleYearParams) ([]Book, error)
func (q *Queries) CreateAuthor(ctx context.Context, name string) (Author, error)
func (q *Queries) CreateBook(ctx context.Context, arg CreateBookParams) (Book, error)
func (q *Queries) DeleteBook(ctx context.Context, bookID int32) error
func (q *Queries) GetAuthor(ctx context.Context, authorID int32) (Author, error)
func (q *Queries) GetBook(ctx context.Context, bookID int32) (Book, error)
func (q *Queries) UpdateBook(ctx context.Context, arg UpdateBookParams) error
func (q *Queries) UpdateBookISBN(ctx context.Context, arg UpdateBookISBNParams) error
func (q *Queries) WithTx(tx *sql.Tx) *Queries
```

## 一度関数だけで考えてみる。

booktestパッケージをbooktestfuncパッケージにコピーして、Queriesのメソッドを全部関数に置き換えた状態でgodocの表示を見てみる。

```console
$ go doc ./booktestfunc
package booktestfunc // import "m/booktestfunc"

func DeleteBook(ctx context.Context, bookID int32) error
func UpdateBook(ctx context.Context, arg UpdateBookParams) error
func UpdateBookISBN(ctx context.Context, arg UpdateBookISBNParams) error
type Author struct{ ... }
    func CreateAuthor(ctx context.Context, name string) (Author, error)
    func GetAuthor(ctx context.Context, authorID int32) (Author, error)
type Book struct{ ... }
    func BooksByTitleYear(ctx context.Context, arg BooksByTitleYearParams) ([]Book, error)
    func CreateBook(ctx context.Context, arg CreateBookParams) (Book, error)
    func GetBook(ctx context.Context, bookID int32) (Book, error)
type BookType string
    const BookTypeFICTION BookType = "FICTION" ...
type BooksByTagsRow struct{ ... }
    func BooksByTags(ctx context.Context, dollar_1 []string) ([]BooksByTagsRow, error)
type BooksByTitleYearParams struct{ ... }
type CreateBookParams struct{ ... }
type NullBookType struct{ ... }
type UpdateBookISBNParams struct{ ... }
type UpdateBookParams struct{ ... }
```

見た目的には悪くない。ただvscodeでの補完は邪魔になる？

## schema部分をinternal/schemaに分ける

Book,Authorなどは全部移動される。

```console
$ go doc ./booktestfunc
package booktestfunc // import "m/booktestfunc"

func BooksByTitleYear(ctx context.Context, arg BooksByTitleYearParams) ([]schema.Book, error)
func CreateAuthor(ctx context.Context, name string) (schema.Author, error)
func CreateBook(ctx context.Context, arg CreateBookParams) (schema.Book, error)
func DeleteBook(ctx context.Context, bookID int32) error
func GetAuthor(ctx context.Context, authorID int32) (schema.Author, error)
func GetBook(ctx context.Context, bookID int32) (schema.Book, error)
func UpdateBook(ctx context.Context, arg UpdateBookParams) error
func UpdateBookISBN(ctx context.Context, arg UpdateBookISBNParams) error
type BooksByTagsRow struct{ ... }
    func BooksByTags(ctx context.Context, dollar_1 []string) ([]BooksByTagsRow, error)
type BooksByTitleYearParams struct{ ... }
type CreateBookParams struct{ ... }
type UpdateBookISBNParams struct{ ... }
type UpdateBookParams struct{ ... }
```

```console
$ go doc ./internal/schema
package schema // import "m/internal/schema"

type Author struct{ ... }
type Book struct{ ... }
type BookType string
    const BookTypeFICTION BookType = "FICTION" ...
type NullBookType struct{ ... }
```

## namespace struct 的な感じでメソッドとして定義してみる

名前空間を分ける方法として幾つか方法が考えられる

- パッケージを分ける
- structを名前空間として使う
- グローバル変数を名前空間として使う

パッケージを分ける以外ではParams系のstructが邪魔になりそう。

### structを名前空間として使う

素直にメソッドを定義。しかしわざわざ毎回newする必要はないのでunexportedなstructを定義し、global変数にわたす

```go
func Foo()
func Bar()
```

を以下のように変換する感じ。分割されたSQLのファイル名などを見れば良いのでは？

```go
type x struct {}
func (x *X) Foo()
func (x *X) Bar()
var X = &x{}
```

go docの内容があんまりうれしくないかもしれない。逆になってほしい。

- メソッドの内容は見えてほしい
- Paramsの内容はあまり見えてほしくない

```go
 go doc ./booktestns/
package booktestns // import "m/booktestns"

var Author = &authorNS{}
var Book = &bookNS{}
type BooksByTagsRow struct{ ... }
type BooksByTitleYearParams struct{ ... }
type CreateBookParams struct{ ... }
type UpdateBookISBNParams struct{ ... }
type UpdateBookParams struct{ ... }
```

bookNSを公開してしまうと、使い方が2通り出てしまう。

- bookNSをnewして使う
- グローバル変数のBookを利用して使う

ただ、今のところ邪魔になるのは、ParamsやRowの型のような気がする。これをどうやって扱うと良いか？

- Builderを定義する
- Parmasの型を公開しない

## Paramsの型を公開せず使う

Paramsの型を公開せず、使うことはできる。警告が出るがunexportedなstructを返す関数を定義することはできる。
どのような関数の名前にするか悩む。

例えば、Doで実行する形にして全部をParamsでwrapする。以下のような使用感。builder用のメソッドがない状態。
それはそれとしてめんどくさい型が補完対象に現れなかったりして楽ではありそう。

```go
{
    p := booktestns.Book.GetBook()
    p.BookID = 1
    book, err := p.Do(ctx)
    fmt.Println(book, err)
}
{
    p := booktestns.Book.UpdateBook()
    p.Title = "foo"
    fmt.Println(p.Do(ctx))
}
```

go docは使い物にならなくなる。

```console
$ go doc -all ./booktestns/
package booktestns // import "m/booktestns"


VARIABLES

var Author = &authorNS{}
var Book = &bookNS{}

TYPES

type BooksByTagsRow struct {
        BookID int32
        Title  string
        Name   sql.NullString
        Isbn   string
        Tags   []string
}
```

## グローバル変数のBookを利用して使う

ちょっと別方向でグローバル変数越しに利用してメソッドを使わないようにしてみる。
具体的には以下のようなコードを書く

```go
var Book struct {
    GetBook func(ctx context.Context, bookID int32) (schema.Book, error)
}

func init(){
    Book.GetBook = getBook    
}
```

go docはごまかせるようになる。やっぱり、paramsが邪魔。

```console
$ go doc -all ./booktestvar
package booktestvar // import "m/booktestvar"


VARIABLES

var Author struct {
	GetAuthor    func(ctx context.Context, authorID int32) (schema.Author, error)
	CreateAuthor func(ctx context.Context, name string) (schema.Author, error)
}
var Book struct {
	UpdateBookISBN   func(ctx context.Context, arg UpdateBookISBNParams) error
	UpdateBook       func(ctx context.Context, arg UpdateBookParams) error
	GetBook          func(ctx context.Context, bookID int32) (schema.Book, error)
	DeleteBook       func(ctx context.Context, bookID int32) error
	CreateBook       func(ctx context.Context, arg CreateBookParams) (schema.Book, error)
	BooksByTitleYear func(ctx context.Context, arg BooksByTitleYearParams) ([]schema.Book, error)
	BooksByTags      func(ctx context.Context, dollar_1 []string) ([]BooksByTagsRow, error)
}

TYPES

type BooksByTagsRow struct {
	BookID int32
	Title  string
	Name   sql.NullString
	Isbn   string
	Tags   []string
}

...
```

あと、sqlxはrequiredな引数とoptionalな引数が存在していないみたい。
補完とかを考えると以下の様な関数になっていると型をわざわざimportしなくても使えるかも？

```go
func (q *BookQueries) UpdateBook(ctx context.Context, modify func(arg *UpdateBookParams)) error {
	var arg UpdateBookParams
	modify(&arg)
	_, err := q.db.ExecContext(ctx, updateBook, arg.Title, pq.Array(arg.Tags), arg.BookID)
	return err
}
```

以下の様な感じで使える。特にUpdate部分は補完で関数部分が展開されるのでParamsを自分で設定しなくて良い。

```go
q := booktestnested2.New(nil)
{
    book, err := q.Book().GetBook(ctx, bookID)
    fmt.Println(book, err)
}

{
    err := q.Book().UpdateBook(ctx, func(arg *booktestnested2.UpdateBookParams) {
        arg.BookID = bookID
        arg.Title = "foo"
    })
    fmt.Println(err)
}
```


## 結局のところInput,Output,Actionを以下のように保持するか？という話なのでは？

そんな気がする。こんな感じ。これらを何度もpackage名などを行ったり来たりせずに扱いたい。

```
Action :: Input => Output
```

どういう形が良いんだろうか？Namespaceが存在していてInput,Action,Outputが手に入れば良いのでは？
```

