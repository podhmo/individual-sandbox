setup:
	pip install openapi-spec-validator

clean:
	docker-compose kill generator codegen
	docker-compose rm  generator codegen
	docker-compose images
run:
	# generate,config-help,meta,langs,version
	docker-compose run --rm codegen generate -i ./openapi3.0.yaml -o dst -v -l python

validate: FILE ?= openapi3.0.yaml
validate:
	docker-compose run --rm -w /project validator ${FILE}
# Available languages: [aspnetcore, csharp, csharp-dotnet2, dynamic-html, html, html2, java, jaxrs-cxf-client, jaxrs-cxf, inflector, jaxrs-cxf-cdi, jaxrs-spec, jaxrs-jersey, jaxrs-di, jaxrs-resteasy-eap, jaxrs-resteasy, spring, nodejs-server, openapi, openapi-yaml, kotlin-client, kotlin-server, php, python, scala, scala-akka-http-server, swift3, swift4, typescript-angular]

web:
	docker-compose up generator
