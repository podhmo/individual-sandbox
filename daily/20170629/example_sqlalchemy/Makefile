run: dog.db gen convert


gen:
	python 01gen.py > gen.json
convert:
	python 02convert.py gen.json > schema

dog.db:
	cat create.sql | sqlite3 -echo dog.db
	cat insert.sql | sqlite3 -echo dog.db

clean:
	rm -f dog.db
