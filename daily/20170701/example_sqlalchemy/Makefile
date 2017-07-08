default: clean
	cat person.sql | sqlite3 -echo person.db
	cat xxx.sql | sqlite3 -echo xxx.db
	cat language.sql | sqlite3 -echo language.db
clean:
	rm -f *.db
