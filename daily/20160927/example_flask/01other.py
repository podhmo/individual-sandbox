import flask
import threading
import contextlib
import logging
import urllib.request
import unittest


store = {"count": 0}


def inc():
    name = flask.request.values.get("name")
    store["count"] += 1
    return flask.jsonify({"message": "inc: {}".format(name)})


def make_app():
    app = flask.Flask(__name__)
    app.config["DEBUG"] = False  # if true, ValueError: signal only works in main thread
    app.config["TESTING"] = True
    logging.basicConfig(level=logging.DEBUG)
    app.add_url_rule("/inc", view_func=inc)
    return app


def make_shutdownable_app(app):
    def shutdown():
        environ = flask.request.environ
        if "werkzeug.server.shutdown" not in environ:
            raise RuntimeError("not running development server")
        environ["werkzeug.server.shutdown"]()
        return flask.jsonify({"message": "halt"})

    app.add_url_rule("/shutdown", view_func=shutdown)
    return app


@contextlib.contextmanager
def with_shutdownable_server(app):
    app = make_shutdownable_app(app)
    t = threading.Thread(target=app.run, kwargs={"port": 4444}, daemon=False)
    t.start()
    import time; time.sleep(1)
    yield
    response = urllib.request.urlopen("http://localhost:{port}/shutdown".format(port=4444))
    print(response.read())
    t.join()


class Tests(unittest.TestCase):
    def test_it(self):
        self.assertEqual(store["count"], 0)
        with with_shutdownable_server(make_app()):
            response = urllib.request.urlopen("http://localhost:{port}/inc?name=bar".format(port=4444))
            print(response.read())
        self.assertEqual(store["count"], 1)

if __name__ == "__main__":
    unittest.main()
