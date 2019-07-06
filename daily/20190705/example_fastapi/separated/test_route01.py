import time
from starlette.testclient import TestClient
from faker import Faker
from app import app


X = 10000
fake = Faker()
client = TestClient(app)


@app.get("/route")
def route():
    bigdata = {
        "key": [
            [fake.email for i in range(X)],
            [fake.name for i in range(X)],
            [fake.random for i in range(X)],
            [fake.password for i in range(X)],
        ]
    }
    return bigdata


def test_routes():
    response = client.get("/route")
    assert response.status_code == 200
