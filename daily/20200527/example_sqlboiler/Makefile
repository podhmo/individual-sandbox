gen: hello.db
	which sqlboiler-sqlite3 || go get github.com/volatiletech/sqlboiler-sqlite3
	go run github.com/volatiletech/sqlboiler sqlite3


hello.db: hello.sql
	cat $< | sqlite3 $@

clean:
	rm -rf models hello.db
.PHONY:
