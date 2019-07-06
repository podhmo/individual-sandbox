import time
import json
from faker import Faker
from typing import List, Any
from pydantic import BaseModel
from fastapi.encoders import jsonable_encoder


X = 10000
fake = Faker()


class BigData(BaseModel):
    key: List[List[Any]]


def route():
    bigdata = {
        "key": [
            [fake.email for i in range(X)],
            [fake.name for i in range(X)],
            [fake.random for i in range(X)],
            [fake.password for i in range(X)],
        ]
    }
    return BigData(**bigdata)


def run():
    st = time.time()
    data = route()
    response = jsonable_encoder(data)
    # print(json.dumps(response))
    print("@@@@@@@@@@", time.time() - st)


run()
