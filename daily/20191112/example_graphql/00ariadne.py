import typing as t
import asyncio
import logging
from handofcats import as_command
from graphql import GraphQLResolveInfo
from ariadne import ObjectType, gql, make_executable_schema, graphql


query = ObjectType("Query")


@query.field("hello")
def resolve_hello(_, info: GraphQLResolveInfo) -> t.Dict[str, t.Any]:
    request = info.context["request"]
    user_agent = request.get("HTTP_USER_AGENT", "guest")
    return {"message": "Hello, %s!" % user_agent}


type_defs = gql(
    """
    type Query {
        hello: Greeting!
    }
    type Greeting {
        message: String!
    }
"""
)


schema = make_executable_schema(type_defs, query)


@as_command
def run():
    logging.basicConfig(level=logging.DEBUG)
    q = {"query": "{ hello { message } }"}
    atask = graphql(
        schema, q, debug=True, context_value={"request": {"HTTP_USER_AGENT": "World"}}
    )
    print(asyncio.run(atask, debug=True))
