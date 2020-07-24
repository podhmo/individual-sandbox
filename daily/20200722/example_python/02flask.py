from flask import Flask, Response
from werkzeug.exceptions import InternalServerError, HTTPException
import unittest


app = Flask(__name__)


@app.route("/")
def hello_world():
    box.append(1)
    box.append(10)
    return "Hello, World!"


@app.route("/ng")
def ng_world():
    box.append(1)
    raise ValueError("hmm")
    box.append(10)
    return "Hello, World!"


box = []


@app.before_request
def on_before_request() -> None:
    global box
    print("before request")
    box.clear()


@app.after_request
def on_after_request(response: Response) -> Response:
    print("after request", response, box)
    box.clear()
    return response


@app.teardown_request
def on_teardown_request(unhandled_exception: Exception):
    print("teardown request", unhandled_exception, box)


@app.errorhandler(Exception)
def on_error(e: InternalServerError):
    if isinstance(e, HTTPException):
        return e
    return f"ng {e}", 500


class Tests(unittest.TestCase):
    def test_ok(self):
        client = app.test_client()
        response = client.get("/")
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response.data, b"Hello, World!")
        print("finished", box)

    def test_ng(self):
        client = app.test_client()
        response = client.get("/ng")
        self.assertEqual(response.status_code, 500)
        print("finished", box)


if __name__ == "__main__":
    # unittest.main()
    app.run()
