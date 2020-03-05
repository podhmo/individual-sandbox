import typing as t
from handofcats import as_command
from graphql import (
    GraphQLSchema,
    GraphQLObjectType,
    GraphQLField,
    GraphQLString,
    GraphQLResolveInfo,
)


def resolve_hello(obj: t.Optional[object], info: GraphQLResolveInfo):
    print(f"{obj=} {info=}")
    return "world"


schema = GraphQLSchema(
    query=GraphQLObjectType(
        name="RootQueryType",
        fields={"hello": GraphQLField(GraphQLString, resolve=resolve_hello)},
    )
)


@as_command
def run() -> None:
    from graphql import graphql_sync

    query = "{ hello }"

    print(graphql_sync(schema, query))
