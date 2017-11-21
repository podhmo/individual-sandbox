import json
from wsgiref.simple_server import make_server
from importlib import import_module, reload


def make_app(filename, transform, usereload):
    def app(environ, start_response):
        status = '200 OK'
        headers = [('Content-type', 'application/json; charset=utf-8')]
        start_response(status, headers)
        with open(filename) as rf:
            d = json.load(rf)

        if transform is not None:
            modname, fnname = transform.rsplit(":", 1)
            m = import_module(modname)
            if usereload:
                m = reload(m)
            fn = getattr(m, fnname)

            transformed = fn(d)
            if transformed is not None:
                d = transformed

        return [json.dumps(d).encode("utf-8")]

    return app


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--port", required=True, type=int)
    parser.add_argument("--transform", default=None)
    parser.add_argument("--reload", action="store_true")
    parser.add_argument("filename")
    args = parser.parse_args()

    httpd = make_server('', args.port, make_app(args.filename, args.transform, args.reload))
    print("Serving on port {}...".format(args.port))
    # Serve until process is killed
    httpd.serve_forever()
