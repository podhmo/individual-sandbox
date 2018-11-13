## golang-migrate golang-migrateからほしそうな情報を取ってみる

https://github.com/golang-migrate/migrate

- migrationsはxxx.up.sql,xxx.down.sql

Driverのinterface

```go
type Driver interface {
	Open(url string) (Driver, error)
	Close() error
	Lock() error
    Unlock() error
    Run(migration io.Reader) error
    SetVersion(version, dirty int) error
    Drop() error
}
```


## python alembic alembicからほしそうな情報を取ってみる

- http://alembic.zzzcomputing.com/

設定ファイルの扱い

.ini

- script location
- timezone
- file_template
- truncate_slug_length
- revision_environment
- sourceless
- version_locations
- output_encoding
- sqlalchemy.url
- logger

これを見ると接続先の情報だけあれば良い気がする？
とはいえ、現状状況に合わせたい

### 生成される情報

- revision
- down_revision
- branch_labels
- upgrade()
- downgrade()

branch labelsだけどういう情報かわかっていない

### command interface

```
# create next migration
revision -m "comment message"

# execute migration or rollback
upgrade head
upgrade <hash>
upgrade +2
downgrade -1

# show current information
current
history
```

## migrations 欲しい機能

- up,downで移動できる
- 現在の位置を記録しておける
- (複数の環境（接続先)に対応できる)
- migration用のtemplateが生成される
- migration scriptを実行できる
- dry runできる
- (次のmigrationをscaffoldできる)
- (migrationの実行順を表示できる)

プロジェクトの構造とかどうしよう？

```
yourproject
  versions/
    xxxxxx_one.py
    xxxxxx_two.py
```
