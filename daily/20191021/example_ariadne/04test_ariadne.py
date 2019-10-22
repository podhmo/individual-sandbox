import pytest
from async_asgi_testclient import TestClient

from ariadne import ObjectType, gql, make_executable_schema
from ariadne.asgi import GraphQL

TestClient.__test__ = False  # prevent PytestCollectionWarning

# Ariadne uses dedicated objects
query = ObjectType("Query")

# Map resolvers to fields in Query type using decorator syntax...
@query.field("hello")
def resolve_hello(_, info):
    request = info.context["request"]
    user_agent = request.get("HTTP_USER_AGENT", "guest")
    return "Hello, %s!" % user_agent


type_defs = gql(
    """
    type Query {
        hello: String!
    }
"""
)

schema = make_executable_schema(type_defs, query)
# As standalone ASGI or WSGI app...
app = GraphQL(schema, debug=True)


@pytest.mark.asyncio
async def test_app():
    client = TestClient(app)

    resp = await client.post("/", json={"query": "{ hello }"})
    assert resp.status_code == 200
    assert resp.json() == {"data": {"hello": "Hello, guest!"}}
