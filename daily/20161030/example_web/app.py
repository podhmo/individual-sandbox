import json
from bottle import route, run
from bottle import request, response
from functools import wraps
import dblib
import authlib


class App:
    def __init__(self, config):
        self.config = config
        self.manager = dblib.Manager(config["store_path"])
        self.users = self.manager.load("users")
        self.auth_service = authlib.default_auth_service(config["auth_salt"])

    # TODO: use reify?
    @property
    def password_auth(self):
        return authlib.PasswordAuthService(self.auth_service, self.users)

    @property
    def jwt_auth(self):
        return authlib.JWTTokenAuthService(self.auth_service, self.users, self.config["jwt_secret"])


def json_response(data):
    response.headers["Content-Type"] = "/application/json"
    return json.dumps(data)


def auth_required(view_fn):
    @wraps(view_fn)
    def wrap(*args, **kwargs):
        auth_header = request.get_header("Authorization")
        if not auth_header.startswith("Bearer "):
            response.set_status(401)
            return json_response({"message": "unauthorized"})
        token = auth_header[len("Bearer "):]
        user = app.jwt_auth.decode(token)
        if user is None:
            response.set_status(401)
            return json_response({"message": "unauthorized"})
        request.user = user
        return view_fn(*args, **kwargs)
    return wrap


@route('/auth/register', method=["POST"])
def register():
    # TODO: need validate
    # uid, name, password
    data = request.json
    user = app.password_auth.signup(data)
    app.users.save()
    return json_response({"token": app.jwt_auth.encode(app.users[user["uid"]])})


@route('/auth/token', method=["POST"])
def token():
    # TODO: need validate
    # name, password
    data = request.json
    found_user = None
    for user in app.users.values():
        if user["name"] == data["name"]:
            found_user = user
    if found_user is None:
        response.set_status(404)
        return json_response({"message": "not found"})
    return json_response({"token": app.jwt_auth.encode(user)})


@route('/me', method=["GET"])
@auth_required
def me():
    return json_response(request.user)


app = App(config={
    "store_path": "./dist",
    "auth_salt": "*me*",
    "jwt_secret": "*jwt*",
})
run(host='localhost', port=8080)
