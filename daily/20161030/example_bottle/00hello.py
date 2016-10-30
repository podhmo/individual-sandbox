import json
from bottle import route, run
from bottle import response


def json_response(data):
    response.headers["Content-Type"] = "/application/json"
    return json.dumps(data)


@route('/', method=["GET"])
def index():
    name = "foo"
    return json_response({"message": "hello: {}".format(name)})


run(host='localhost', port=8080)
