00: clean sample.db
	go run main$@.go

01: clean sample.db
	diff -u main00.go main$@.go > 00$@.diff || :
	go run main$@.go

02: clean sample.db
	diff -u main00.go main$@.go > 00$@.diff || :
	go run main$@.go

clean:
	rm -rf sample.db
sample.db:
	cat insert.sql | sqlite3 $@
