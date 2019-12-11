from fastapi import FastAPI, APIRouter, Form
from pydantic import BaseModel


router = APIRouter()


class HelloOutput(BaseModel):
    message: str


@router.post("/hello", response_model=HelloOutput)
def hello(name: str = Form(...)):
    return {"message": f"Hello, {name}"}


app = FastAPI(debug=True)
app.include_router(router)


async def main() -> None:
    from async_asgi_testclient import TestClient
    from dictknife import loading

    async with TestClient(app) as client:
        response = await client.get("/openapi.json")
        loading.dumpfile(response.json())


if __name__ == "__main__":
    import asyncio
    import os

    asyncio.run(main(), debug=bool(os.environ.get("DEBUG")))
