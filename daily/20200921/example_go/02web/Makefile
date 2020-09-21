default: sample.db
	go run main.go

sample.db:
	cat insert.sql | sqlite3 sample.db
clean:
	rm -f sample.db
.PHONY: clean
