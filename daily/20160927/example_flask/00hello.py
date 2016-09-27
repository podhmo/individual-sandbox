import flask


def hello():
    name = flask.request.values.get("name")
    return flask.jsonify({"message": "hello: {}".format(name)})


def main():
    app = flask.Flask(__name__)
    app.add_url_rule("/hello", view_func=hello)
    app.run(port=4444)

if __name__ == "__main__":
    main()
