gen:
	# $(MAKE) start
	python gen.py --src "mysql+pymysql://root@127.0.0.1:43306/sakila" | tee graphql-schema.json

setup:
	pip install sqlalchemy pymysql

