package main

import (
	"fmt"

	"github.com/blastrain/vitess-sqlparser/sqlparser"
	"github.com/k0kubun/pp"
)

// ValType specifies the type for SQLVal.
type ValType int

// These are the possible Valtype values.
// HexNum represents a 0x... value. It cannot
// be treated as a simple value because it can
// be interpreted differently depending on the
// context.
const (
	StrVal = ValType(iota) // 0
	IntVal
	FloatVal
	HexNum
	HexVal
	ValArg // 5
)

func main() {
	stmt, err := sqlparser.Parse("select * from user_items where user_id=? AND user_id=1 order by created_at limit 3 offset 10")
	if err != nil {
		panic(err)
	}
	pp.Println(stmt)
	fmt.Printf("stmt = %+v\n", stmt)
}
