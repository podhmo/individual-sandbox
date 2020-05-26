book.db: insert.sql
	cat $< | sqlite3 $@

clean:
	rm -f book.db
.PHONY: clean
