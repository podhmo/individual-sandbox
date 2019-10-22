import asyncio
from graphql import GraphQLSchema, GraphQLObjectType, GraphQLField, GraphQLString


async def resolve_hello(obj, info):
    print("start resolve hello")
    await asyncio.sleep(1)
    print("finish resolve hello")
    return "world"


schema = GraphQLSchema(
    query=GraphQLObjectType(
        name="RootQueryType",
        fields={"hello": GraphQLField(GraphQLString, resolve=resolve_hello)},
    )
)


def main() -> None:
    from graphql import graphql

    async def run():
        query = "{ hello }"
        print(await graphql(schema, query))

    asyncio.run(run(), debug=True)


if __name__ == "__main__":
    main()
