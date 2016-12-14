from flask import Flask
from something import Something
from flask import g, current_app
from werkzeug.local import LocalProxy

# separated module (views)
from flask import Blueprint
b = Blueprint("hello", __name__)


@b.route("/")
def hello():
    return hmm.hello()


# separated module (utilities)
def find_hmm():
    print("hoi")
    if not hasattr(g, "hmm"):
        print("hai")
        g.hmm = Something(current_app)
    return g.hmm

hmm = LocalProxy(find_hmm)


# app
def make_app():

    class settings:
        MESSAGE = "hello from something"

    app = Flask(__name__)
    app.config.from_object(settings)
    app.register_blueprint(b)
    return app


if __name__ == "__main__":
    app = make_app()
    app.run(port=4040)
