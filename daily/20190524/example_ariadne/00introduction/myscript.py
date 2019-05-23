import logging
from ariadne import QueryType, gql, make_executable_schema
from ariadne.asgi import GraphQL


logger = logging.getLogger(__name__)

type_defs = gql(
    """
type Query {
  hello: String!
}
"""
)

# Create type instance for Query type defined our schema...
query = QueryType()

# ... and assign our resolver function to its 'hello' field.
@query.field("hello")
def resolve_hello(_, info) -> str:
    request = info.context["request"]
    user_agent = request.headers.get("user-agent", "guest")
    return f"Hello, {user_agent}!"


logging.basicConfig(level=logging.DEBUG)
schema = make_executable_schema(type_defs, query)
app = GraphQL(schema, debug=True)
