import logging
import time
from typing import List, Any

from faker import Faker
from pydantic import BaseModel
from starlette.testclient import TestClient
from timing_asgi import TimingClient, TimingMiddleware
from timing_asgi.integrations import StarletteScopeToName

from fastapi import FastAPI

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class PrintTimings(TimingClient):
    def timing(self, metric_name, timing, tags):
        logger.info(f"{metric_name}, {timing}, {tags}")


fake = Faker()
app = FastAPI()
client = TestClient(app)


app.add_middleware(
    TimingMiddleware,
    client=PrintTimings(),
    metric_namer=StarletteScopeToName(prefix="app", starlette_app=app),
)

X = 10000


@app.get("/route1")
def route1():
    t0 = time.time()
    bigdata = {
        "key": [
            [fake.email for i in range(X)],
            [fake.name for i in range(X)],
            [fake.random for i in range(X)],
            [fake.password for i in range(X)],
        ]
    }
    t1 = time.time()
    logger.info(f"route1: {t1-t0}")
    return bigdata


class BigData(BaseModel):
    key: List[List[Any]]


@app.get("/route2", response_model=BigData)
def route2():
    t0 = time.time()
    bigdata = {
        "key": [
            [fake.email for i in range(X)],
            [fake.name for i in range(X)],
            [fake.random for i in range(X)],
            [fake.password for i in range(X)],
        ]
    }
    t1 = time.time()
    logger.info(f"route1: {t1-t0}")
    return bigdata


@app.get("/route3", response_model=BigData)
def route3():
    t0 = time.time()
    bigdata = {
        "key": [
            [fake.email for i in range(X)],
            [fake.name for i in range(X)],
            [fake.random for i in range(X)],
            [fake.password for i in range(X)],
        ]
    }

    t1 = time.time()
    logger.info(f"route1: {t1-t0}")
    return BigData(**bigdata)


def test_routes():
    response = client.get("/route1")
    assert response.status_code == 200
    response = client.get("/route2")
    assert response.status_code == 200
    response = client.get("/route3")
    assert response.status_code == 200
