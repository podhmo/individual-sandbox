import flask
from flask import request, jsonify
from marshmallow import Schema, fields, ValidationError
from handofcats import as_command


class UserSchema(Schema):
    name = fields.String(required=True)
    age = fields.Integer()


app = flask.Flask(__name__)


@app.route("/", methods=["POST"])
def register():
    payload = request.json
    try:
        data = UserSchema().load(payload)
    except ValidationError as e:
        return jsonify(message=str(e)), 400
    return jsonify(data), 200


@as_command
def run(*, port: int) -> None:
    app.run(debug=True, port=port)
