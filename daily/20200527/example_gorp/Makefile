00:
	go run $(shell echo $@*)/main.go
01:
	go run $(shell echo $@*)/main.go

book.db: insert.sql
	cat $< | sqlite3 $@

clean:
	rm -f book.db
.PHONY: clean
