from typing import List, Any
from starlette.testclient import TestClient
from pydantic import BaseModel
from faker import Faker
from app import app


X = 10000
fake = Faker()
client = TestClient(app)


class BigData(BaseModel):
    key: List[List[Any]]

    @classmethod
    def validate(cls, value):
        return super().validate(value)


@profile
def run():
    bigdata = {
        "key": [
            [fake.email for i in range(X)],
            [fake.name for i in range(X)],
            [fake.random for i in range(X)],
            [fake.password for i in range(X)],
        ]
    }
    s = BigData(**bigdata)
    return s.dict()


def main():
    print(len(run()))


main()
