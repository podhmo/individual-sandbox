# from: https://ariadnegraphql.org/docs/starlette-integration.html
from ariadne import QueryType, make_executable_schema, gql
from ariadne.asgi import GraphQL
from starlette.applications import Starlette

type_defs = gql(
    """
type Query {
  hello: String!
}
"""
)

query = QueryType()


@query.field("hello")
def resolve_hello(_, info):
    return "Hello, World"


schema = make_executable_schema(type_defs, query)

app = Starlette(debug=True)
app.mount("/graphql", GraphQL(schema, debug=True))
