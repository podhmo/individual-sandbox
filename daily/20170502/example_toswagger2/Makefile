default: schema.py

schema.py: swagger.yaml
	swagger-marshmallow-codegen $< > $@

swagger.yaml: config-sample.json
	python ../example_toswagger/toswagger.py --show-minimap --name conf $< > $@

config-sample.json:
	wget https://raw.githubusercontent.com/nfarina/homebridge/master/config-sample.json
