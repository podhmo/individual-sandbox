run:
	PYTHONPATH=. python app/__init__.py

test:
	bash test.sh

default: route schema

route:
	pyramid-swagger-router swagger.yaml .

schema:
	swagger-marshmallow-codegen swagger.yaml > app/schema.py

