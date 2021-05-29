schema: pokemon.db
	echo '.schema data' |  sqlite3 $<

00: pokemon.db
	go run $(shell echo $@*/)main.go

pokemon.db:
	python gendb.py --input-file data/pokemon.csv --output-file pokemon.db --db-name data
