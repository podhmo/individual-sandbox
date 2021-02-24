package main

import "fmt"

type Node interface{ node() }

type Bop string

const (
	BopEQ  Bop = "$eq"
	BopGT      = "$gt"
	BopGTE     = "$gte"
	BopIN      = "$in"
	BopLT      = "$lt"
	BopLTE     = "$lte"
	BopNE      = "$ne"
	BopNIN     = "$nin"
)

type B struct {
	Op    Bop
	Value interface{}
}

func (b B) node() {}

type Mop string

const (
	MopAND   Mop = "$and"
	MopNOT       = "$not"
	MopNOR       = "$nor"
	MopOR        = "$or"
	MopEMPTY     = ""
)

type M struct {
	Op Mop

	Values []Node
}

func (m M) node() {}

type Named struct {
	Name  string
	Value Node
}

func (n Named) node() {}

type Bool bool

func (b Bool) node() {}

func main() {
	{
		fmt.Println("")
		fmt.Println(`db.inventory.find( { qty: { $gt: 20 } } )`)
		m := M{Values: []Node{
			Named{Name: "qty", Value: B{Op: BopGT, Value: 20}},
		}}
		fmt.Println(m)
	}
	{
		fmt.Println("")
		fmt.Println(`db.inventory.find( { xxx.qty: { $gt: 20 } } )`)
		m := M{Values: []Node{
			Named{Name: "xxx.qty", Value: B{Op: BopGT, Value: 20}},
		}}
		fmt.Println(m)
	}

	// // 無理
	// {
	// 	fmt.Println("")
	// 	fmt.Println(`db.inventory.find( { qty: { $gt: 20, $lt: 50  } } )`)
	// 	m := M{Values: []Node{
	// 		Named{Name: "qty", Value: B{Op: BopGT, Value: 20}},
	// 	}}
	// 	fmt.Println(m)
	// }
	{
		fmt.Println("")
		fmt.Println(`db.inventory.find( { xxx: {qty: { $gt: 20 } }, "yyy": { $eq: 10  } } )`)
		m := M{
			Values: []Node{
				Named{Name: "xxx", Value: M{
					Values: []Node{
						Named{Name: "qty", Value: B{Op: BopGT, Value: 20}},
					},
				}},
				Named{Name: "yyy", Value: B{Op: BopEQ, Value: 10}},
			},
		}
		fmt.Println(m)
	}
	{
		fmt.Println("")
		fmt.Print(`
db.inventory.find( {
    $and: [
        { $or: [ { qty: { $lt : 10 } }, { qty : { $gt: 50 } } ] },
        { $or: [ { sale: true }, { price : { $lt : 5 } } ] }
    ]
} )
`)
		m := M{
			Values: []Node{
				M{
					Op: MopAND,
					Values: []Node{
						M{
							Op: MopOR,
							Values: []Node{
								Named{Name: "qty", Value: B{Op: BopLT, Value: 10}},
								Named{Name: "qty", Value: B{Op: BopGT, Value: 50}}},
						},
						M{
							Op: MopOR,
							Values: []Node{
								Named{Name: "sale", Value: Bool(true)},
								Named{Name: "price", Value: B{Op: BopLT, Value: 5}}},
						},
					},
				},
			},
		}
		fmt.Println(m)
	}
}
