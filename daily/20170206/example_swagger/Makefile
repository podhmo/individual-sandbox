default: route schema

route:
	pyramid-swagger-router swagger.yaml .

schema:
	swagger-marshmallow-codegen swagger.yaml > app/schema.py

