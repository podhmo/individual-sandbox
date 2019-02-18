setup: src/petstore.2.0.json src/petstore.3.0.json
src:
	mkdir -p src
src/petstore.2.0.json: src
	wget https://mermade.org.uk/examples/swagger.json -O $@
src/petstore.3.0.json: src
	wget https://mermade.org.uk/examples/openapi.json -O $@

dst:
	mkdir -p dst
00: dst
	dictknife diff -o jsonpatch src/{2,3}.0.yaml | tee dst/00basic.patch.json
	dictknife diff -o jsonpatch src/petstore.{2,3}.0.json | tee dst/00petstore.patch.json
01: dst
	# xxx: savedir is not good parameters. emitting onto dst/01/src/01
	python 2to3.py --src src/2.0.yaml --savedir dst/01 --dry-run
