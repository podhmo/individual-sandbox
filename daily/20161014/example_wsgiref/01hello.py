from wsgiref.simple_server import make_server


def hello_world_app(environ, start_response):
    status = '200 OK'
    headers = [('Content-type', 'text/plain; charset=utf-8')]
    start_response(status, headers)
    return ["Hello World"]

httpd = make_server('', 8000, hello_world_app)
httpd.serve_forever()
