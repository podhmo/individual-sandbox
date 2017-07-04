src/groups.db:
	cat src/create.sql | sqlite3 -echo $@
	cat src/insert.sql | sqlite3 -echo $@

clean:
	rm -f src/*.db
