from flask import Flask
import unittest


app = Flask(__name__)


@app.route("/")
def hello_world():
    return "Hello, World!"


class Tests(unittest.TestCase):
    def test_it(self):
        client = app.test_client()
        response = client.get("/")
        self.assertEqual(response.data, b"Hello, World!")


if __name__ == "__main__":
    unittest.main()
