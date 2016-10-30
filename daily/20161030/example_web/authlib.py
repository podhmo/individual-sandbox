import string
import random
import hashlib
import jwt
import copy
from functools import wraps


class Salter:
    def __init__(self, appsalt, size=12):
        self.appsalt = appsalt
        self.size = size

    def get_salt(self):
        return "".join([random.choice(string.printable) for _ in range(self.size)])

    def fulldata(self, token, salt=None):
        salt = salt or self.get_salt()
        return {"salt": salt, "token": self.generate(token, salt)}

    def generate(self, token, salt):
        return "{}{}".format(token, salt)


class Hasher:
    def __init__(self, size=10):
        self.size = size

    def fulldata(self, token, stretch_times=None):
        stretch_times = stretch_times or self.get_stretch_times()
        return {"token": self.generate(token, stretch_times), "stretch_times": stretch_times}

    def generate(self, token, stretch_times):
        if hasattr(token, "encode"):
            token = token.encode("utf-8")
        for _ in range(stretch_times - 1):
            token = hashlib.sha384(token).digest()
        return hashlib.sha384(token).hexdigest()

    def get_stretch_times(self):
        return random.randint(2, self.size)


class TokenGenerator:
    def __init__(self, salter, hasher):
        self.salter = salter
        self.hasher = hasher

    def fulldata(self, password, salt=None, stretch_times=None):
        data = self.salter.fulldata(password, salt=salt)
        data.update(self.hasher.fulldata(data["token"], stretch_times=stretch_times))
        return data

    def generate(self, password, salt, stretch_times):
        return self.hasher.generate(self.salter.generate(password, salt), stretch_times)


class AuthService:
    def __init__(self, tokengen):
        self.tokengen = tokengen

    def create_user(self, uid, password):
        data = self.tokengen.fulldata(password)
        data["uid"] = uid
        data["password"] = data.pop("token")
        return data

    def check_password(self, user, password):
        password = self.tokengen.generate(password, user["salt"], user["stretch_times"])
        return user["password"] == password


def default_auth_service(appsalt):
    return AuthService(TokenGenerator(Salter(appsalt, 12), Hasher(10)))


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
        return jwt.encode(result, self.jwt_secret, algorithm="HS256").decode("utf-8")

    @public_user_data_only
    def decode(self, token):
        print(token, "@")
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


if __name__ == "__main__":
    service = default_auth_service(":secret:")
    user = service.create_user("1", "password")
    print(user)
    print(service.check_password(user, "password"))
