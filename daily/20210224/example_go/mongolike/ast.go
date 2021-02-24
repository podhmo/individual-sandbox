package mongolike

type Comparison = string

const (
	ComparisonEQ  Comparison = "$eq"
	ComparisonGT             = "$gt"
	ComparisonGTE            = "$gte"
	ComparisonIN             = "$in"
	ComparisonLT             = "$lt"
	ComparisonLTE            = "$lte"
	ComparisonNE             = "$ne"
	ComparisonNIN            = "$nin"
)

type Logical = string

const (
	LogicalAND Logical = "$and"
	LogicalNOT Logical = "$not"
	LogicalNOR Logical = "$nor"
	LogicalOR  Logical = "$or"
)

type M map[string]interface{}

// db.inventory.find( { qty: { $gt: 20 } } )
M{"qty": {"$gt": 20}}
// db.inventory.find( { xxx.qty: { $gt: 20 } } )
// db.inventory.find( { xxx: {qty: { $gt: 20 } }, "yyy": { $eq: 10  } } )
