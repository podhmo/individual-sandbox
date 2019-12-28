from fastapi import FastAPI

app = FastAPI()


@app.get("/")
def hello():
    return {"hello": "world"}


def main():
    import asyncio
    from async_asgi_testclient import TestClient

    async def run():
        async with TestClient(app) as c:
            response = await c.get("/")
            print("@", response, response.json())

    asyncio.run(run(), debug=True)


if __name__ == "__main__":
    main()
