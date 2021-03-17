module m

go 1.15

require (
	github.com/go-gorp/gorp/v3 v3.0.2 // indirect
	github.com/jmoiron/sqlx v1.3.1
	github.com/mattn/go-sqlite3 v1.14.6
	github.com/pkg/errors v0.9.1
	github.com/podhmo/tenuki v0.0.4
	github.com/rs/zerolog v1.20.0
	github.com/simukti/sqldb-logger v0.0.0-20201125162808-c35f87e285f2
	github.com/simukti/sqldb-logger/logadapter/zerologadapter v0.0.0-20201125162808-c35f87e285f2
)

replace github.com/podhmo/tenuki => ../../../../../../../ghq/github.com/podhmo/tenuki

replace github.com/jmoiron/sqlx => ../../../../../../../ghq/github.com/jmoiron/sqlx
