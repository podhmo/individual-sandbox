import graphql
from graphql.utilities import print_schema

Person = graphql.GraphQLObjectType(
    "Person",
    lambda: {
        "name": graphql.GraphQLField(graphql.GraphQLNonNull(graphql.GraphQLString,)),
    },
)
Query = graphql.GraphQLObjectType(
    "Query",
    lambda: {
        "people": graphql.GraphQLField(
            graphql.GraphQLNonNull(graphql.GraphQLList(Person))
        )
    },
)
schema = graphql.GraphQLSchema(Query)


print(print_schema(schema))
# or graphql.print_schema(schema)
