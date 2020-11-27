00: file.db
	sqlite3 file.db

# init
file.db: insert.sql
	cat	$< | sqlite3 $@
clean:
	rm -f *.db
