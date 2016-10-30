import jwt
import copy
import uuid
import hashlib
from functools import wraps
import auth


def public_user_data_only(fn):
    @wraps(fn)
    def _wrap(*args, **kwargs):
        data = copy.copy(fn(*args, **kwargs))
        data.pop("stretch_times", None)
        data.pop("salt", None)

        # maskingの方が良いかも？
        data.pop("password", None)
        return data
    return _wrap


class JWTTokenAuthService:
    def __init__(self, auth_service, user_repository, jwt_secret):
        self.auth_service = auth_service
        self.user_repository = user_repository
        self.jwt_secret = jwt_secret

    def encode(self, user):
        result = {"uid": user["uid"]}
        return jwt.encode(result, self.jwt_secret, algorithm="HS256")

    @public_user_data_only
    def decode(self, token):
        data = jwt.decode(token, self.jwt_secret)
        user = self.user_repository[data["uid"]]
        return user


class PasswordAuthService:
    def __init__(self, auth_service, user_repository):
        self.auth_service = auth_service
        self.user_repository = user_repository

    def signup(self, user):
        data = copy.deepcopy(user)
        data.update(self.auth_service.create_user(data["uid"], data["password"]))
        self.user_repository[data["uid"]] = data
        return data

    def _get_user_by_name(self, name):
        for user in self.user_repository.values():
            if name == user["name"]:
                return user
        return None

    @public_user_data_only
    def login(self, name, password):
        user = self._get_user_by_name(name)
        if user is None:
            return None
        if not self.auth_service.check_password(user, password):
            return None
        return user


class TwoPhaseSignupAuthService:
    def __init__(self, auth_service, user_repository, draft_user_repository):
        self.auth_service = auth_service
        self.user_repository = user_repository
        self.draft_user_repository = draft_user_repository

    def generate_token(self, name):
        token = name + uuid.uuid4().hex
        return hashlib.sha1(token.encode("utf-8")).hexdigest()

    def register(self, user):
        data = copy.deepcopy(user)
        data.update(self.auth_service.create_user(data["uid"], data["password"]))
        token = self.generate_token(data["name"])
        self.draft_user_repository[token] = data
        return token

    def find_draft_user(self, token):
        return self.draft_user_repository.get(token)

    @public_user_data_only
    def signup(self, token):
        duser = self.find_draft_user(token)
        if duser is None:
            return None
        self.draft_user_repository.pop(token)
        user = self.user_repository[duser["uid"]] = duser
        return user

if __name__ == "__main__":
    auth_service = auth.default_auth_service(":secret:")
    user_repository = {}
    draft_user_repository = {}
    twophase_signup_service = TwoPhaseSignupAuthService(auth_service, user_repository, draft_user_repository)
    password_auth_service = PasswordAuthService(auth_service, user_repository)
    token_auth_service = JWTTokenAuthService(auth_service, user_repository, ":jwt:")

    postdata = {"uid": "1", "name": "foo", "password": "password"}
    print("post data:", postdata)
    tmp_token = twophase_signup_service.register(postdata)
    print("tmp token:", tmp_token)
    print("signup:", twophase_signup_service.signup(tmp_token))

    print("login:", password_auth_service.login("foo", "password"))

    token = token_auth_service.encode(user_repository["1"])
    print("encoded: ", token)

    user = token_auth_service.decode(token)
    print("decoded: ", user)
