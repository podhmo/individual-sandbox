import pytest
import async_asgi_testclient as aat


async def app(scope, receive, send):
    # assert scope["type"] == "http"
    await send(
        {
            "type": "http.response.start",
            "status": 200,
            "headers": [[b"content-type", b"text/plain"]],
        }
    )
    await send({"type": "http.response.body", "body": b"Hello, world!"})


@pytest.mark.asyncio
async def test_app():
    async with aat.TestClient(app) as client:
        resp = await client.get("/")
        assert resp.status_code == 200
        assert resp.text == "Hello, world!"
