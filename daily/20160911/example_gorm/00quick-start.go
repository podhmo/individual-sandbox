package main

import (
	"github.com/jinzhu/gorm"
	_ "github.com/jinzhu/gorm/dialects/sqlite"
	"log"
)

type Product struct {
	gorm.Model
	Code  string `gorm:"unique_index:idx_product_code"`
	Price uint
}

func setup(db *gorm.DB) {
    db.LogMode(true)

	// migrate schema
	db.AutoMigrate(&Product{})
    // db.Model(&Product{}).AddUniqueIndex("idx_product_code", "code") // tagで設定しても良い
}

func run(db *gorm.DB) {
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

func main() {
	db, err := gorm.Open("sqlite3", "00quick-start.db")
	if err != nil {
		log.Fatal(err)
	}
	setup(db)
	run(db)
	defer db.Close()

}
