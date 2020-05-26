gen:
	which sqlboiler-sqlite3 || go get github.com/volatiletech/sqlboiler-sqlite3
	go run github.com/volatiletech/sqlboiler sqlite3


hello.db: hello.sql
	cat $< | sqlite3 $@
