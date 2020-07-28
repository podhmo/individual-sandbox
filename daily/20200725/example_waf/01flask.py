import os
from flask import Flask

app = Flask(__name__)


@app.route("/", defaults={"path": ""}, methods=["POST", "GET"])
@app.route("/<path:path>", methods=["POST", "GET"])
def all(path):
    return {"path": path}


app.run(port=os.environ["PORT"])
