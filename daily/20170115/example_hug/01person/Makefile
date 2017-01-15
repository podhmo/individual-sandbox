list:
	hug -f app.py -c list_people

NAME ?= "foo"
AGE ?= 100

create:
	hug -f app.py -c create_person ${NAME} ${AGE}

server:
	PYTHONPATH=. uwsgi --http 0.0.0.0:8000 --wsgi-file app.py --callable __hug_wsgi__


api_list:
	http --pretty=format GET http://localhost:8000/people

api_create:
	echo '{"name": "boo", "age": 10}' | http --json --verbose POST http://localhost:8000/people
