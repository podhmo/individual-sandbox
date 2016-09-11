# golang database/sql

orm使わない状態で何ができるかも把握しておきたい感ある。

- [sql - The Go Programming Language](https://golang.org/pkg/database/sql/)
- [Go database/sql tutorial](http://go-database-sql.org/)

driverをテキトウに選んでimportしないとダメ。

```go
// sqliteの場合
import (
	"database/sql"
	_ "github.com/mattn/go-sqlite3"
)
```

## tracer

これが面白い。

- [Go言語でSQLのトレースをする - Shogo's Blog](https://shogo82148.github.io/blog/2015/05/13/golang-sql-proxy/)

そう言えば、sql.DBとsql.Txが両方渡せるinterfaceはdatabase/sqlには定義されていないっぽい？

```go
type Preparer interface {
    Prepare(query string) (*sql.Stmt, error)
}

func run(db Preparer) error {
}

tx, err := db.Begin()
if err != nil {
    log.Fatal(err)
}
defer tx.Rollback()
err = run(tx)
```

# golang gormの使い方

- [example_gorm](example_gorm)
- [GORM Guide](http://jinzhu.me/gorm/) の内容を見ていけばある程度普通に使えそう

gormわりと文字列ベースであんまり型の力は利用できなさそうな感じ。

## 思ったこと

- tagの仕様をどうにかしてcompletion可能な感じにしたいな〜。
- tagの取得は GetModelStruct付近のコードっぽい。

# sqlite foreignkey constraint

sqlite add constraint文サポートしていないじゃん

- [SQLite Foreign Key Support](https://www.sqlite.org/foreignkeys.html)
- [SQL Features That SQLite Does Not Implement](https://www.sqlite.org/omitted.html)
- [sql - How do I add a foreign key to an existing sqlite (3.6.21) table? - Stack Overflow](http://stackoverflow.com/questions/1884818/how-do-i-add-a-foreign-key-to-an-existing-sqlite-3-6-21-table)

# データ分析

- [機械学習によるデータ分析まわりのお話](http://www.slideshare.net/canard0328/ss-44288984)
- [組織のなかで働く技術 - やしお](http://d.hatena.ne.jp/Yashio/20160910/1473500203)
