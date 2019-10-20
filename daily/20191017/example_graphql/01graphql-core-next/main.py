from graphql import GraphQLSchema, GraphQLObjectType, GraphQLField, GraphQLString

schema = GraphQLSchema(
    query=GraphQLObjectType(
        name="RootQueryType",
        fields={
            "hello": GraphQLField(GraphQLString, resolve=lambda obj, info: "world")
        },
    )
)


def main() -> None:
    from graphql import graphql_sync

    query = "{ hello }"

    print(graphql_sync(schema, query))


if __name__ == "__main__":
    main()
