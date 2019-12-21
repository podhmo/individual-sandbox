from starlette.testclient import TestClient


from app import app


# use starlette starlette==0.12.9, error
def test_middleware():
    client = TestClient(app)
    response = client.get("/")
    assert "X-Process-Time" in response.headers
