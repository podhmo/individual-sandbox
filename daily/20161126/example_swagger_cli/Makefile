SWAGGER = ./node_modules/.bin/swagger


zero:
	${SWAGGER} validate ./00onefile/swagger.yaml

one:
	${SWAGGER} validate ./01separated/swagger.yaml

two:
	${SWAGGER} bundle --dereference --outfile ./02multifile/swagger.json ./02multifile/src/main.yaml
	${SWAGGER} validate ./02multifile/swagger.json
