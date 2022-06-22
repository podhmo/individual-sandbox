00:
	python main.py

data.db:
	cat insert.sql | sqlite3 $@
clean:
	rm -f data.db
