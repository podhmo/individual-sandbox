import typing as t
import inspect
from fastapi import FastAPI
from starlette.requests import Request
from starlette.responses import HTMLResponse

app = FastAPI()


def ui(template: t.Callable[[t.Dict[str, t.Any]], str]):
    def decorate(fn):
        print(inspect.signature(fn))

        def wrapped(request: Request):
            data = fn()
            if "ui" in request.query_params:
                return HTMLResponse(template(request, data))
            return data

        return wrapped

    return decorate


@app.get("/")
@ui(template=lambda r, d: f"<p>{d['message']}</p>")
def hello():
    return {"message": "hello world"}


def main():
    import asyncio
    from async_asgi_testclient import TestClient

    async def run():
        async with TestClient(app) as c:
            response = await c.get("/")
            print("GET /     ", response, response.json())

            response = await c.get("/", query_string={"ui": "1"})
            print("GET /?ui=1", response, response.text)

    asyncio.run(run(), debug=True)


if __name__ == "__main__":
    main()
