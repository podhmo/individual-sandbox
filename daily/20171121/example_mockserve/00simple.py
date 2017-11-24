import json
from wsgiref.simple_server import make_server


def make_app(filename, transform, usereload):
    def app(environ, start_response):
        status = '200 OK'
        headers = [('Content-type', 'application/json; charset=utf-8')]
        start_response(status, headers)
        with open(filename) as rf:
            d = json.load(rf)

        d["fullName"] = "{d[firstName]} {d[lastName]}".format(d=d)

        return [json.dumps(d).encode("utf-8")]

    return app


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--port", required=True, type=int)
    parser.add_argument("filename")
    args = parser.parse_args()

    httpd = make_server('', args.port, make_app(args.filename))
    print("Serving on port {}...".format(args.port))
    # Serve until process is killed
    httpd.serve_forever()
