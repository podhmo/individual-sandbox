import typesystem


class BaseUser:
    username: str
    is_admin: bool


class User(BaseUser, typesystem.Schema):
    username = typesystem.String(max_length=100)
    is_admin = typesystem.Boolean(default=False)
