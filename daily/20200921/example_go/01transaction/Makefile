default: sample.db
	go run main.go -db $<

sample.db:
	cat insert.sql | sqlite3 sample.db

clean:
	rm -rf sample.db
.PHONY: clean

# dump:
# echo .dump | sqlite3 02*.sqlite | tee $(shell echo 02*.sqlite).sql
