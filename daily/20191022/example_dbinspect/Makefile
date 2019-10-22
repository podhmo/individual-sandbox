00:
	# need db-start
	python $@*.py
01:
	sqlacodegen 'mysql+pymysql://root@127.0.0.1:43306/sakila'
02:
	# need poetry
	cd foo && poetry run sqlacodegen 'mysql+pymysql://root@127.0.0.1:43306/sakila'
03:
	python $@*.py

db-start:
	$(MAKE) -f db.mk start
db-stop:
	$(MAKE) -f db.mk start

setup:
	pip install -r requirements.txt
