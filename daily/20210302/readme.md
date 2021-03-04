## go で create table (schema) の部分はどうなっている？

この辺のやつら

- https://entgo.io/docs/migrate/
- https://github.com/xo/xo
- https://github.com/go-gorp/gorp
- https://github.com/go-gorm/gorm

```console
$ ghq get facebook/ent
$ ghq get xo/xo
$ ghq get go-gorp/gorp
```

### gorp

- CreateTablesIfNotExists() からスタート https://github.com/go-gorp/gorp/blob/master/db.go#L432
- https://github.com/go-gorp/gorp/blob/master/table.go#L173
- 差分はdialetに逃がす https://github.com/go-gorp/gorp/blob/master/dialect.go#L14
- 差分の実装例 https://github.com/go-gorp/gorp/blob/master/dialect_postgres.go#L14

結構素直に愚直に実装されていた。

### ent

- schemaのCreatorを実装するらしい https://github.com/ent/ent/blob/master/dialect/sql/schema/migrate.go#L78
- 実態としてはMigrateのstructのcreate()が呼ばれる https://github.com/ent/ent/blob/master/dialect/sql/schema/migrate.go#L154
- init,types,txCreate()してからcommitが呼ばれる
- visitorと言う程でもないけれど、差分はsqlDialectというinterfaceで吸収 https://github.com/ent/ent/blob/master/dialect/sql/schema/migrate.go#L637
- 実装例とテスト https://github.com/ent/ent/blob/master/dialect/sql/schema/postgres.go https://github.com/ent/ent/blob/master/dialect/sql/schema/postgres_test.go

もう一段回キレイにwrapしている感ある（複雑）。gremlinなどにも対応しているせいかもしれない。

### gorm

- Migratorというinterfaceがある https://github.com/go-gorm/gorm/blob/master/migrator.go#L33
- 分岐は既に終わってsturctにデータが入っている前提 https://github.com/go-gorm/gorm/blob/master/migrator/migrator.go#L155
- 方言部分の吸収は別リポジトリ。実装例。と言ってもbaseのやつを実行したあとのコメント程度のようだ https://github.com/go-gorm/postgres/blob/master/migrator.go

### xorm

- Engine.DumpTables()のときにSQLが出力される https://gitea.com/xorm/xorm/src/branch/master/engine.go#L1034 
- DialectのCreateTableSQLで文字列を生成している https://gitea.com/xorm/xorm/src/branch/master/engine.go#L562
- Dialectはこれ https://gitea.com/xorm/xorm/src/branch/master/dialects/dialect.go#L42
- 実装例はこの辺、愚直にそれぞれの方言ごとに実装している https://gitea.com/xorm/xorm/src/branch/master/dialects/postgres.go#L901 base部分はないわけではない https://gitea.com/xorm/xorm/src/branch/master/dialects/dialect.go#L76
- ただし謎がある（？）
- それとは別にEngine.CreateTables()があり、これはsession.createTable()を呼ぶ(?) https://gitea.com/xorm/xorm/src/branch/master/engine.go#L1034

## 名前

quoterか

## xorm ちょっと面白い記述

とおもったけど。そうでもない？

- baseのメソッドからからpostgresのメソッドは dialectフィールド経由で触る
- postgresのメソッドからbaseのメソッドは 埋め込みなので触れる
- 外部から触るときは、Dialectというinterfaceとして postgresのメソッドを呼ぶ

```go
// Base represents a basic dialect and all real dialects could embed this struct
type Base struct {
	dialect Dialect
	uri     *URI
	quoter  schemas.Quoter
}

func (db *Base) IsColumnExist(queryer core.Queryer, ctx context.Context, tableName, colName string) (bool, error) {
	quote := db.dialect.Quoter().Quote
	query := fmt.Sprintf(
		"SELECT %v FROM %v.%v WHERE %v = ? AND %v = ? AND %v = ?",
		quote("COLUMN_NAME"),
		quote("INFORMATION_SCHEMA"),
		quote("COLUMNS"),
		quote("TABLE_SCHEMA"),
		quote("TABLE_NAME"),
		quote("COLUMN_NAME"),
	)
	return db.HasRecords(queryer, ctx, query, db.uri.DBName, tableName, colName)
}

type postgres struct {
	Base
}

func (db *postgres) IsColumnExist(queryer core.Queryer, ctx context.Context, tableName, colName string) (bool, error) {
	args := []interface{}{db.getSchema(), tableName, colName}
	query := "SELECT column_name FROM INFORMATION_SCHEMA.COLUMNS WHERE table_schema = $1 AND table_name = $2" +
		" AND column_name = $3"
	if len(db.getSchema()) == 0 {
		args = []interface{}{tableName, colName}
		query = "SELECT column_name FROM INFORMATION_SCHEMA.COLUMNS WHERE table_name = $1" +
			" AND column_name = $2"
	}

	rows, err := queryer.QueryContext(ctx, query, args...)
	if err != nil {
		return false, err
	}
	defer rows.Close()

	return rows.Next(), nil
}

// Dialect represents a kind of database
type Dialect interface {
	Init(*URI) error
	URI() *URI
	SQLType(*schemas.Column) string
	FormatBytes(b []byte) string

	IsReserved(string) bool
	Quoter() schemas.Quoter
	SetQuotePolicy(quotePolicy QuotePolicy)

	AutoIncrStr() string

	GetIndexes(queryer core.Queryer, ctx context.Context, tableName string) (map[string]*schemas.Index, error)
	IndexCheckSQL(tableName, idxName string) (string, []interface{})
	CreateIndexSQL(tableName string, index *schemas.Index) string
	DropIndexSQL(tableName string, index *schemas.Index) string

	GetTables(queryer core.Queryer, ctx context.Context) ([]*schemas.Table, error)
	IsTableExist(queryer core.Queryer, ctx context.Context, tableName string) (bool, error)
	CreateTableSQL(table *schemas.Table, tableName string) ([]string, bool)
	DropTableSQL(tableName string) (string, bool)

	GetColumns(queryer core.Queryer, ctx context.Context, tableName string) ([]string, map[string]*schemas.Column, error)
	IsColumnExist(queryer core.Queryer, ctx context.Context, tableName string, colName string) (bool, error)
	AddColumnSQL(tableName string, col *schemas.Column) string
	ModifyColumnSQL(tableName string, col *schemas.Column) string

	ForUpdateSQL(query string) string

	Filters() []Filter
	SetParams(params map[string]string)
}
```


## SQL to struct, function?

https://future-architect.github.io/articles/20200728/

何か期待するそれとは違った

## terraform


1047975870
2047975870
    

https://dev.classmethod.jp/articles/my-terraform-tips/

## python selenium

pythonにselenium moduleなんてあったんだ

```console
$ brew install chromedriver
$ pip install selenium
```

- https://selenium-python.readthedocs.io/getting-started.html
