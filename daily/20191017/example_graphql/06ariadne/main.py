import asyncio
import logging
from ariadne import ObjectType, gql, make_executable_schema, graphql


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


logging.basicConfig(level=logging.DEBUG)
schema = make_executable_schema(type_defs, query)

query = {"query": "{ hello }"}
atask = graphql(
    schema, query, debug=True, context_value={"request": {"HTTP_USER_AGENT": "World"}}
)
print(asyncio.run(atask, debug=True))
