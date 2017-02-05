run:
	PYTHONPATH=. python app/__init__.py

test:
	bash test.sh

default: route schema format


route:
	pyramid-swagger-router swagger.yaml .

schema:
	swagger-marshmallow-codegen swagger.yaml > app/schema.py

format:
	yapf -i app/schema.py
