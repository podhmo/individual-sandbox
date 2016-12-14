from flask import Flask
from something import Something
from flask import current_app

# separated module (views)
from flask import Blueprint
b = Blueprint("hello", __name__)


@b.route("/")
def hello():
    return hmm.hello()


# separated module (utilities)
class LazyOnceEvalObject(object):
    def __init__(self, fn):
        self._fn = fn
        self.proxy = None

    def __getattr__(self, name):
        if self.proxy is None:
            self.proxy = self._fn()
        print("hai")
        return getattr(self.proxy, name)


def find_hmm():
    print("hoi")
    return Something(current_app)

hmm = LazyOnceEvalObject(find_hmm)


# app
def make_app():

    class settings:
        MESSAGE = "hello from something"

    app = Flask(__name__)
    app.config.from_object(settings)
    app.register_blueprint(b)

    with app.app_context():
        hmm.hello()
    return app


if __name__ == "__main__":
    app = make_app()
    app.run(port=4040)
