from wsgiref.simple_server import make_server


def app(environ, start_response):
    status = "200 OK"
    headers = [("Content-type", "application/json; charset=utf-8")]
    start_response(status, headers)
    import time

    time.sleep(8)
    return [b'{"message": "hello world"}']


with make_server("", 8080, app) as http:
    print("Serving on port 8080...")
    http.serve_forever()
