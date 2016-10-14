from werkzeug.serving import run_simple
from hello_wsgi import application
run_simple('localhost', 8000, application, use_reloader=False)
