from __future__ import annotations
import typing as t
import typing_extensions as tx
import dataclasses
from flask import request, Request, Flask
from marshmallow import Schema, fields, ValidationError
from handofcats import as_command


class Accessor(tx.Protocol):
    @property
    def paths(self) -> GetItem:
        ...

    @property
    def queries(self) -> GetItem:
        ...

    @property
    def headers(self) -> GetItem:
        ...

    @property
    def cookies(self) -> GetItem:
        ...

    def formdata(self) -> GetItem:
        ...

    def jsondata(self) -> GetItem:
        ...


class GetItem(tx.Protocol):
    def __getitem__(self, k: t.Any) -> t.Any:
        ...


class GetMultiItem(tx.Protocol):
    def __getitem__(self, k: t.Any) -> str:
        ...

    def getlist(self, key: t.Any) -> t.List[str]:
        ...


class FlaskAccessor:
    def __init__(self, req: Request, *, path_params: t.Dict[str, t.Any]) -> None:
        self.request = req
        self.path_params = path_params

    @property
    def paths(self) -> GetItem:
        return self.path_params

    @property
    def queries(self) -> GetMultiItem:
        return self.request.args

    @property
    def headers(self) -> GetItem:
        return self.request.headers

    @property
    def cookies(self) -> GetItem:
        return self.request.cookies

    def formdata(self) -> GetMultiItem:
        return self.request.form

    def jsondata(self) -> GetItem:
        return self.request.json


# see
# - path
# - queries
# - headers
# - cookies
# - form data
# - body

T = t.TypeVar("T")


def parse(
    fn: t.Callable[[Accessor], T],
    req: Request,
    path_params: t.Optional[t.Dict[str, t.Any]] = None,
) -> T:
    try:
        return fn(FlaskAccessor(req, path_params=path_params or {}))
    except ValidationError:
        raise  # TODO


class UserSchema(Schema):
    name = fields.String(required=True)
    age = fields.Integer()


@dataclasses.dataclass
class RegisterUserInput:
    service_name: str
    user: t.Dict

    @classmethod
    def parse(cls, a: Accessor) -> RegisterUserInput:
        # check header
        # check queries
        name = a.paths["name"]

        payload = t.cast(t.Mapping[str, t.Any], a.jsondata())
        user = UserSchema().load(payload)
        return RegisterUserInput(
            service_name=name,
            user=user,
        )


app = Flask(__name__)


def register(**path_params):
    input = parse(RegisterUserInput.parse, request, path_params=path_params)
    return UserSchema().dump(input.user), 200


@as_command
def run(*, port: int) -> None:
    app.route("/service/{name}/users", methods=["POST"])(register)
    app.run(debug=True, port=port)
