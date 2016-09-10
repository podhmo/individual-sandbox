# gormの使い方

[GORM Guide](http://jinzhu.me/gorm/)

## install

```
go get -u github.com/jinzhu/gorm
# sqlite3
go get -u github.com/jinzhu/gorm/dialects/sqlite
```

## 調べること

- quick start([Getting Started with GORM · GORM Guide](http://jinzhu.me/gorm/))
- simple crud
- test
- join, preload
- transaction

## quick start

### db dialectの設定

- sqliteを使う場合は以下の様にする

```go
import (
	"github.com/jinzhu/gorm"
	_ "github.com/jinzhu/gorm/dialects/sqlite"
)
```

### modelの定義

- gorm.Modelを埋め込むとmodelになる

```go
type Product struct {
    gorm.Model
    Code string
    Price uint
}
```

### schema migration

- autoMigrate()にmodelを渡すとdb schemaのmigrationしてくれる

```go
func gen(db *gorm.DB){
    // migrate schema
    db.AutoMigrate(&Product{})
}
```

### dbアクセスの方法

```go
db, err := gorm.Open("sqlite3", "<db name>")
// do something
defer db.Close()
```

### CRUD

- create -- db.Create()
- read -- db.First(), db.Model()
- update -- db.Update()
- delete -- db.Delete()

```go
func crud(db *gorm.DB) {
	// create
    log.Println("create")
	db.Create(&Product{Code: "L1212", Price: 1000})

    // read
    var product Product
    log.Println("read")
    log.Printf("\t%v\n", db.First(&product, 1)) // find product with id 1
    log.Printf("\t%v\n", db.First(&product, "code = ?", "L1212")) // find product with code L1212

    // update
    log.Println("update")
    log.Printf("\t%v\n", db.Model(&product).Update("Price", 2000)) // update product's price to 2000

    // delete
    db.Delete("delete")
    log.Printf("\t%v\n", db.Delete(&product))
}
```

### 謎

- unique制約の追加
- prepared statement
