package main

import "fmt"

type Mop = string

const (
	MopEQ  Mop = "$eq"
	MopGT      = "$gt"
	MopGTE     = "$gte"
	MopIN      = "$in"
	MopLT      = "$lt"
	MopLTE     = "$lte"
	MopNE      = "$ne"
	MopNIN     = "$nin"
)

const (
	MopAND Mop = "$and"
	MopNOT     = "$not"
	MopNOR     = "$nor"
	MopOR      = "$or"
)

type M map[string]interface{}

func main() {
	{
		// db.inventory.find( { qty: { $gt: 20 } } )
		m := M{"qty": M{MopGT: 20}}
		fmt.Println(m)
	}
	{
		// db.inventory.find( { xxx.qty: { $gt: 20 } } )
		m := M{"xxx.qty": M{MopGT: 20}}
		fmt.Println(m)
	}
	{
		// db.inventory.find( { xxx: {qty: { $gt: 20 } }, "yyy": { $eq: 10  } } )
		m := M{
			"xxx": M{"qty": M{MopGT: 20}},
			"yyy": M{"$eq": 10},
		}
		fmt.Println(m)
	}
	{
		// db.inventory.find( {
		//     $and: [
		//         { $or: [ { qty: { $lt : 10 } }, { qty : { $gt: 50 } } ] },
		//         { $or: [ { sale: true }, { price : { $lt : 5 } } ] }
		//     ]
		// } )

		m := M{
			"$and": []M{
				M{"$or": []M{
					M{"qty": M{"$lt": 10}},
					M{"qty": M{"$gt": 50}},
				}},
				M{"$or": []M{
					M{"sale": true},
					M{"price": M{"$lt": 5}},
				}},
			},
		}
		fmt.Println(m)
	}

}
