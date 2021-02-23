import asyncio
import fastapi
import uvicorn
from handofcats import as_command

api = fastapi.FastAPI()


@api.get("/")
def hello():
    return {"message": "hello world"}


@as_command
def run():
    server = uvicorn.Server(config=uvicorn.Config(api))
    asyncio.run(server.serve())
