# wip gormの使い方

- [GORM Guide](http://jinzhu.me/gorm/)

## install

```
go get -u github.com/jinzhu/gorm
# sqlite3
go get -u github.com/jinzhu/gorm/dialects/sqlite
```

## 調べること

- quick start([Getting Started with GORM · GORM Guide](http://jinzhu.me/gorm/))
- model relation
- simple crud
- test
- join, preload
- transaction

## quick start

quick start([Getting Started with GORM · GORM Guide](http://jinzhu.me/gorm/)) の内容見ながら分かったことをメモしてく

### db dialectの設定

- sqliteを使う場合は以下の様にする
- [他のdialectについて](http://jinzhu.me/gorm/database.html)
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

    // mysqlなどでdb engine変えたい場合など
    // db.Set("gorm:table_options", "ENGINE=InnoDB").AutoMigrate(&Product{})
}
```

その他細かくDDL実行したい場合には以下の様なものがある

- db.CreateTable()
- db.DropTable()
- db.ModifyColumn()
- db.DropColumn()
- db.AddForeignkey()

indexなどの指定は 以下の様にする？

```go
exes

// Add index for columns `name` with given name `idx_user_name`
db.Model(&User{}).AddIndex("idx_user_name", "name")

// Add index for columns `name`, `age` with given name `idx_user_name_age`
db.Model(&User{}).AddIndex("idx_user_name_age", "name", "age")

// Add unique index
db.Model(&User{}).AddUniqueIndex("idx_user_name", "name")

// Add unique index for multiple columns
db.Model(&User{}).AddUniqueIndex("idx_user_name_age", "name", "age")

// Remove index
db.Model(&User{}).RemoveIndex("idx_user_name")
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

- [x] unique制約の追加
- [x] prepared statement
- [x] model同士のrelationの定義の仕方(e.g. one to many...)
- [x] tableのinsert時のdefault値を設定(e.g. updated_atを常にtime.Now())
- [x] transactionを貼りたい
- [x] error時などに特別な処理をしたい
- 実行されたSQLのlogのようなものを表示したい

## model relation

- [Models · GORM Guide](http://jinzhu.me/gorm/models.html#model-definition)

以下のようなrelationを定義

```
user -  credit_card
user -* email
user - address
user *-* language
```

- 型の設定はtagで設定する(e.g. `gorm:"size:255"`, `gorm:"many2many:user_languages`)
- auto increment用の型などは無い
- not null constraintもtag
- unique constraintもtag
- primary keyの指定もtagもしくは、 `BeforeCreate()` メソッドを定義
- default値の指定もtag (有効にするには `gorm:"default:<default value>"`)
- defaultはsnake_caseの名前を使わない。(有効にするには `gorm:"column:<table_name>"`)

```go
type User struct {
    gorm.Model
    Birthday     time.Time
    Age          int
    Name         string  `gorm:"size:255"` // Default size for string is 255, reset it with this tag
    Num          int     `gorm:"AUTO_INCREMENT"`

    CreditCard        CreditCard      // One-To-One relationship (has one - use CreditCard's UserID as foreign key)
    Emails            []Email         // One-To-Many relationship (has many - use Email's UserID as foreign key)

    BillingAddress    Address         // One-To-One relationship (belongs to - use BillingAddressID as foreign key)
    BillingAddressID  sql.NullInt64

    ShippingAddress   Address         // One-To-One relationship (belongs to - use ShippingAddressID as foreign key)
    ShippingAddressID int

    IgnoreMe          int `gorm:"-"`   // Ignore this field
    Languages         []Language `gorm:"many2many:user_languages;"` // Many-To-Many relationship, 'user_languages' is join table
}

type Email struct {
    ID      int
    UserID  int     `gorm:"index"` // Foreign key (belongs to), tag `index` will create index for this column
    Email   string  `gorm:"type:varchar(100);unique_index"` // `type` set sql type, `unique_index` will create unique index for this column
    Subscribed bool
}

type Address struct {
    ID       int
    Address1 string         `gorm:"not null;unique"` // Set field as not nullable and unique
    Address2 string         `gorm:"type:varchar(100);unique"`
    Post     sql.NullString `gorm:"not null"`
}

type Language struct {
    ID   int
    Name string `gorm:"index:idx_name_code"` // Create index with name, and will create combined index if find other fields defined same name
    Code string `gorm:"index:idx_name_code"` // `unique_index` also works
}

type CreditCard struct {
    gorm.Model
    UserID  uint
    Number  string
}
```

## query

- http://jinzhu.me/gorm/curd.html

queryには幾つか種類がある

- First() 先頭
- Last() 末尾
- Find() 全部

条件

- Where() 始点
- Or()
- Not()
- And()

色々追加

- Select()
- Order()
- Limit()
- Offset()
- Count()
- Group()
- Having()
- Joins()
- Preload()

hmm

- 別の結果(struct)にmappingするには `db.Scan()`
- ちょっとした抽象なものを利用するのは `db.Scopes()`
- select for updateみたいな条件指定は `db.Set()`


```go
db.Set("gorm:query_option", "FOR UPDATE).first(&user, 10)
// SELECT * from user where id = 10 FOR UPDATE;
```

※ get or create的なものもあるらしい(FirstOrInit(), FirstOrCreate())

## transaction

- http://jinzhu.me/gorm/advanced.html

transactionにはdb.Beginというものがあるらしい。その内部では db.<Method>ではなく tx.<Method> のような形で使う

```go
// begin a transaction
tx := db.Begin()

// do some database operations in the transaction (use 'tx' from this point, not 'db')
tx.Create(...)

// ...

// rollback the transaction in case of error
tx.Rollback()

// Or commit the transaction
tx.Commit()
```

## error時などに特別な処理をしたい

- http://jinzhu.me/gorm/advanced.html

gorm.DBが色々状態を持っている。

- Error
- RecordNotFound


```go
if err := db.Where("name = ?", "jinzhu").First(&user).Error; err != nil {
    // error handling...
}

// If there are more than one error happened, get all of them with `GetErrors`, it returns `[]error`
db.First(&user).Limit(10).Find(&users).GetErrors()

// Check if returns RecordNotFound error
db.Where("name = ?", "hello world").First(&user).RecordNotFound()

if db.Model(&user).Related(&credit_card).RecordNotFound() {
    // no credit card found handling
}
```

## 雑にsqlの結果を見たい

- http://jinzhu.me/gorm/advanced.html

LogModeを使えば良い

```go
db.LogMode(true)
```
