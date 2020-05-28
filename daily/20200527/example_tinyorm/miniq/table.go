package miniq

type Table string

func (t Table) Query(
	options ...func(*QueryImpl),
) *QueryImpl {
	q := &QueryImpl{
		FromClause:   From(t),
		SelectClause: Select(STAR),
		WhereClause:  Where(),
	}
	for _, opt := range options {
		opt(q)
	}
	return q
}

func (Table) Select(fields ...Field) func(*QueryImpl) {
	return func(q *QueryImpl) {
		q.SelectClause = Select(fields...)
	}
}

func (Table) Where(ops ...op) func(*QueryImpl) {
	return func(q *QueryImpl) {
		q.WhereClause = Where(ops...)
	}
}
