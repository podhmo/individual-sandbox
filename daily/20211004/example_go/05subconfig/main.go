package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"m/05subconfig/config/configanotherDB"
	"m/05subconfig/config/configdb"
	"m/05subconfig/lib/db"
)

type Config struct {
	// 全部埋め込みで作れる
	// packageを分けている理由はあって、例えば特定の状況でだけ特定のミドルウェアへの依存が現れたときに共通の依存として含めたくない
	configdb.DBConfig
	configanotherDB.AnotherDBConfig
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	c := new(Config)
	if err := json.NewDecoder(bytes.NewBufferString(`
{
	"db": {"uri": "localhost:9200"},
	"anotherDB": {"uri": "localhost:9200"}
}
`)).Decode(c); err != nil {
		return err
	}
	return use(c)
}

func use(provider interface {
	DB() *db.DB
	AnotherDB() *db.DB
}) error {
	// TODO: validation

	fmt.Println(provider.DB())
	fmt.Println(provider.AnotherDB())
	return nil
}
