# from: https://github.com/graphql-python/graphql-core-next
import asyncio
from graphql import (
    graphql,
    GraphQLSchema,
    GraphQLObjectType,
    GraphQLField,
    GraphQLString,
)


async def resolve_hello(obj, info):
    print("hmm...")
    await asyncio.sleep()
    print("...!")
    return "world"


schema = GraphQLSchema(
    query=GraphQLObjectType(
        name="RootQueryType",
        fields={"hello": GraphQLField(GraphQLString, resolve=resolve_hello)},
    )
)


async def main():
    query = "{ hello }"
    print("Fetching the result...")
    result = await graphql(schema, query)
    print(result)


asyncio.run(main(), debug=True)
