waitress:
	waitress-serve --port 8000 hello_wsgi:application 2>/dev/null
gunicorn:
	gunicorn 05hello:application
uwsgi:
	uwsgi --http :8000 --wsgi-file 05hello.py 2>/dev/null
aio:
	python 02*.py 2> /dev/null
py3:
	python3.5 00hello.py 2> /dev/null
py2:
	python2.7 01hello.py 2> /dev/null
bench:
	ab -n 6 -n 1000 http://localhost:8000/
