from wsgiref.simple_server import make_server
import socketserver
import selectors
socketserver._ServerSelector = selectors.DefaultSelector


def app(environ, start_response):
    status = '200 OK'
    headers = [('Content-type', 'text/plain; charset=utf-8')]
    start_response(status, headers)
    return [b"Hello World"]

httpd = make_server('', 8000, app)
httpd.serve_forever()
