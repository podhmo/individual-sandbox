run: greetings.db
	python main.py

greetings.db:
	cat insert.sql | sqlite3 $@

clean: greetings.db
	rm -f $^
