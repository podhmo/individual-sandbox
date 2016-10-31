import jwt
import copy


jwt_load = jwt.PyJWT()._load


def create_user():
    user = {
        "id": "1",
        "name": "foo",
        "auth": {
            "token_salt": "*hekeheke*"
        }
    }
    return user


def save_user(store, user):
    store[user["id"]] = user


def token_from_user(user):
    payload = copy.deepcopy(user)
    payload.pop("auth")
    token = jwt.encode(payload, user["auth"]["token_salt"], headers={"uid": user["id"]})
    print("encoded:", token)
    return token


def user_from_token(store, token):
    payload, signing_input, headers, signature = jwt_load(token)
    print("headers:", headers)
    user = store[headers["uid"]]
    decoded = jwt.decode(token, user["auth"]["token_salt"])
    print("decoded:", decoded)
    return user


if __name__ == "__main__":
    store = {}
    user = create_user()
    save_user(store, user)
    token = token_from_user(user)
    user_from_token(store, token)
