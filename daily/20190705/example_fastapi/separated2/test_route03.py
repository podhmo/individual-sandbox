from typing import List, Any
import time
from starlette.testclient import TestClient
from pydantic import BaseModel
from faker import Faker
from app import app


X = 10000
fake = Faker()
client = TestClient(app)


class BigData(BaseModel):
    key: List[List[Any]]


@app.get("/route", response_model=BigData)
def route3():
    bigdata = {
        "key": [
            [fake.email for i in range(X)],
            [fake.name for i in range(X)],
            [fake.random for i in range(X)],
            [fake.password for i in range(X)],
        ]
    }
    return BigData(**bigdata)


def test_routes():
    response = client.get("/route")
    assert response.status_code == 200
