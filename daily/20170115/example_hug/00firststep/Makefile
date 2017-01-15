python:
	PYTHONPATH=. python -c 'from app import happy_birthday; print(happy_birthday("foo", 20))'

cmd:
	hug -f app.py -c help || echo ok
	hug -f app.py -c happy_birthday foo 20

web:
	PYTHONPATH=. uwsgi --http 0.0.0.0:8000 --wsgi-file app.py --callable __hug_wsgi__
