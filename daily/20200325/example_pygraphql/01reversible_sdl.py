import graphql
from graphql.language import print_ast

# # Build a GraphQLSchema from an introspection result.
# build_client_schema,
# # Build a GraphQLSchema from a parsed GraphQL Schema language AST.
# build_ast_schema,
# # Build a GraphQLSchema from a GraphQL schema language document.
# build_schema,


type_defs = """
type Person {
    name: String!
}

type Query {
    people: [Person]!
}
"""
document_node = graphql.parse(type_defs)
print(print_ast(document_node))

# schema = graphql.build_schema(type_defs)

