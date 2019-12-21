import pytest


from app import app


@pytest.mark.asyncio
async def test_middleware2():
    from async_asgi_testclient import TestClient

    async with TestClient(app) as client:
        response = await client.get("/")
        assert "X-Process-Time" in response.headers
