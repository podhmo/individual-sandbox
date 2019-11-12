import asyncio
import logging
import strawberry
from strawberry.asgi import GraphQL
from async_asgi_testclient import TestClient
from handofcats import as_command


@strawberry.type
class Greeting:
    message: str


@strawberry.type
class Query:
    @strawberry.field
    def hello(self, info) -> Greeting:
        request = info.context["request"]
        user_agent = request.get("HTTP_USER_AGENT", "guest")
        return Greeting(message=f"Hello, {user_agent}!")


schema = strawberry.Schema(query=Query)


@as_command
def run():
    logging.basicConfig(level=logging.DEBUG)
    app = GraphQL(schema, debug=True)

    async def atask():
        client = TestClient(app)
        q = {"query": "{ hello { message } }"}
        resp = await client.post("/", json=q)
        print(resp.status_code) == 200
        print(resp.json())

    asyncio.run(atask(), debug=True)
