SWAGGER = node_modules/.bin/swagger


zero:
	${SWAGGER} validate ./00onefile/swagger.yaml

one:
	${SWAGGER} validate ./01separated/swagger.yaml

two:
	${SWAGGER} bundle --dereference --outfile ./02multifile/swagger.json ./02multifile/src/main.yaml
	${SWAGGER} validate ./02multifile/swagger.json

three:
	(cd ./03multifile/src/ ../../${SWAGGER} bundle --outfile ../swagger.json --format 2 main.yaml)
	${SWAGGER} validate ./03multifile/swagger.json

four:
	swagger-bundler bundle ./04bundler/src/main.yaml > ./04bundler/swagger.yaml
	${SWAGGER} validate ./04bundler/swagger.yaml
