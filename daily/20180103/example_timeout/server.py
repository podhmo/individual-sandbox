import time
from wsgiref.simple_server import make_server


def app(environ, start_response):
    status = '200 OK'
    headers = [('Content-type', 'text/plain; charset=utf-8')]
    for i in range(100):
        print(i)
        time.sleep(1)
    start_response(status, headers)
    return [b"Hello World"]


httpd = make_server('', 8000, app)
print("Serving on port 8000...")

httpd.serve_forever()
