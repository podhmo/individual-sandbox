00:
	python $@*.py
01:
	python $@*.py
02:
	python $@*.py
02-dump:
	echo .dump | sqlite3 02*.sqlite | tee $(shell echo 02*.sqlite).sql
03:
	python $@*.py

setup:
	pip install -r requirements.txt
clean:
	rm -f *.sqlite*
