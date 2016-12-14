# http://flask.pocoo.org/docs/0.11/testing/
import json
import unittest
from flask import Flask
from flask import Blueprint
from flask import request, jsonify, current_app


app = Blueprint("hello", __name__)


@app.route("/")
def hello():
    return jsonify({
        "message": current_app.config["MESSAGE"],
        "name": request.args.get("name"),
        "special": getattr(request, "special", None)
    })


class Tests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.app = Flask(__name__)

        class MyRequest(cls.app.request_class):
            special = "SPeCIAL"

        cls.app.request_class = MyRequest

        cls.app.config["MESSAGE"] = "FAKE"
        cls.app.register_blueprint(app)

    def test_it(self):
        with self.app.test_request_context("/?name=foo"):
            request.special = "special"
            response = self.app.full_dispatch_request()
            data = response.get_data(as_text=True)
            self.assertEqual(json.loads(data), {"name": "foo", "message": "FAKE", "special": "special"})

            with self.app.test_request_context("/?name=foo"):
                response = self.app.full_dispatch_request()
                data = response.get_data(as_text=True)
                self.assertEqual(json.loads(data), {"name": "foo", "message": "FAKE", "special": "SPeCIAL"})


if __name__ == "__main__":
    unittest.main()
