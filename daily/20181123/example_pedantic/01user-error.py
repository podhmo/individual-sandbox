from datetime import datetime
from typing import List
from pydantic import (
    BaseModel,
    ValidationError,
)


class User(BaseModel):
    id: int
    name: str = "Jone Doe"
    signup_ts: datetime = None
    friends: List[int] = []


data = {
    "signup_ts": "2017-06-01 12:22",
    "friends": [1, 2, "not number"],
}

try:
    user = User(**data)
except ValidationError as e:
    print(e.json())

# [
#   {
#     "loc": [
#       "id"
#     ],
#     "msg": "field required",
#     "type": "value_error.missing"
#   },
#   {
#     "loc": [
#       "friends",
#       2
#     ],
#     "msg": "value is not a valid integer",
#     "type": "type_error.integer"
#   }
# ]
