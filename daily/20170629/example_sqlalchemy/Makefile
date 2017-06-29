run: dog.db
	ptyhon 00run.py

dog.db:
	cat create.sql | sqlite3 -echo dog.db
	cat insert.sql | sqlite3 -echo dog.db

clean:
	rm -f dog.db
