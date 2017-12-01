import json
import urllib.parse as parselib
from wsgiref.simple_server import make_server


def on_get(environ, start_response):
    path = environ["PATH_INFO"]
    print(parselib.parse_qs(environ["QUERY_STRING"]))


def on_post_rest(environ, start_response):
    wsgi_input = environ["wsgi.input"]
    content_length = int(environ["CONTENT_LENGTH"])
    # data: JSON = Dict[string, Union[string,int,float,bool,JSON,List[JSON]]]
    data = json.loads(wsgi_input.read(content_length))


def app(environ, start_response):
    status = '200 OK'
    headers = [('Content-type', 'application/json; charset=utf-8')]
    start_response(status, headers)
    return [b"Hello World"]


httpd = make_server('', 8000, app)
print("Serving on port 8000...")

# Serve until process is killed
httpd.serve_forever()
