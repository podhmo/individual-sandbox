setup: fetch copy schema

fetch:
	git clone --depth=1 git@github.com:hjacobs/connexion-example.git || echo "ok"

copy:
	cp connexion-example/{app.py,test.sh,requirements.txt,swagger.yaml} .

schema:
	swagger-marshmallow-codegen --logging DEBUG swagger.yaml > schema.py
