from flask import Flask
from something import Something


# separated module (views)
from flask import Blueprint
b = Blueprint("hello", __name__)


@b.route("/")
def hello():
    return hmm.hello()


# app
def make_app():

    class settings:
        MESSAGE = "hello from something"

    app = Flask(__name__)
    app.config.from_object(settings)
    hmm = Something(app)  # これに触る方法がない
    app.register_blueprint(b)

    return app


if __name__ == "__main__":
    app = make_app()
    app.run(port=4040)
