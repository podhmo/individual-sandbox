run:
	PYTHONPATH=. python app/__init__.py

views:
	pyramid-swagger-router swagger.yaml .

fetch:
	wget https://raw.githubusercontent.com/hjacobs/connexion-example/master/swagger.yaml
	gsed -i 's@app.@app.views.@' swagger.yaml

fetch_test:
	wget https://raw.githubusercontent.com/hjacobs/connexion-example/master/test.sh
