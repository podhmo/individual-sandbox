from starlette.applications import Starlette
from starlette.requests import Request
from starlette.responses import JSONResponse
from starlette.routing import Route
from starlette.exceptions import HTTPException
from marshmallow import Schema, fields, ValidationError


class UserSchema(Schema):
    name = fields.String(required=True)
    age = fields.Integer()


async def register(request: Request):
    payload = await request.json()
    try:
        data = UserSchema().load(payload)
    except ValidationError as e:
        raise HTTPException(status_code=400, detail=str(e))
    return JSONResponse(UserSchema().dump(data))


app = Starlette(
    debug=True,
    routes=[
        Route("/", register, methods=["POST"]),
    ],
)
