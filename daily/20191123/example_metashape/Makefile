00:
	python $@*.py
01:
	python $@*.py
02:
	python $@*.py
03:
	python $@*.py
04: openapi-mini.json
	python -m metashape.inputs.python openapi-mini.json | tee 04openapi.py
05:
	cd $@*/ && uvicorn main:app --debug
05-client:
	http -b http://localhost:8000/openapi.json
06:
	python $@*.py

openapi-mini.json: openapi.yaml
	jsonknife cut --src openapi.yaml --ref "#/components" --ref "#/paths" | dictknife cat -i yaml --dst openapi-mini.json
openapi.yaml:
	wget https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/samples/openapi3/server/petstore/go-api-server/api/openapi.yaml

