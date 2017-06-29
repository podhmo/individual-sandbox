run: dog.db
	python 00run.py

gen:
	python 01gen.py > gen.json

dog.db:
	cat create.sql | sqlite3 -echo dog.db
	cat insert.sql | sqlite3 -echo dog.db

clean:
	rm -f dog.db
