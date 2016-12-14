from flask import Flask
from something import Something


class settings:
    MESSAGE = "hello from something"

app = Flask(__name__)
app.config.from_object(settings)
hmm = Something(app)


@app.route("/")
def hello():
    print(hmm)
    return hmm.hello()


if __name__ == "__main__":
    app.run(port=4040)
