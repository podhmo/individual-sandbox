00: data.db
	python $(shell echo $@*.py)
01:
	python $(shell echo $@*.py)

data.db:
	cat insert.sql | sqlite3 data.db
