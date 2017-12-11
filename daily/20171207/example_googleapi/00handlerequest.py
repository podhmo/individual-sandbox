from wsgiref.simple_server import make_server


def make_oauth_dance_server(callback):
    def app(environ, start_response):
        status = '200 OK'
        headers = [('Content-type', 'text/plain; charset=utf-8')]
        callback(environ)
        start_response(status, headers)
        return [b"ok"]

    httpd = make_server('', 44444, app)  # http://localhost:44444/
    return httpd


def callback(environ):
    # http "http://localhost:44444/?code=4/md7SamLaYb8ejx9agxcV8xv8VmmkYwxzc7wPp2lYBIw"
    from urllib.parse import parse_qs
    qs = parse_qs(environ["QUERY_STRING"])
    print(qs)


httpd = make_oauth_dance_server(callback)
httpd.handle_request()
