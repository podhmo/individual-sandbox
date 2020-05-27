package mgox

func Where(ops ...op) *WhereClause {
	values := make([]interface{}, len(ops))
	for i, op := range ops {
		values[i] = op
	}

	value := And(values...)
	value.WithoutParen = true
	return &WhereClause{Prefix: "WHERE", Value: value}
}

type WhereClause struct {
	Prefix string
	Value  *Mop
}
