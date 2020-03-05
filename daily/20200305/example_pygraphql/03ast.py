from graphql import parse
from graphql import print_ast

document = parse(
    """
    type Query {
      me: User
    }

    type User {
      id: ID
      name: String
    }
    """,
    no_location=True,
)
print(document)
print(print_ast(document))

# ast

from graphql.language.ast import (
    DocumentNode,
    ObjectTypeDefinitionNode,
    NameNode,
    FieldDefinitionNode,
    NamedTypeNode,
)

document = DocumentNode(
    definitions=[
        ObjectTypeDefinitionNode(
            name=NameNode(value="Query"),
            fields=[
                FieldDefinitionNode(
                    name=NameNode(value="me"),
                    type=NamedTypeNode(name=NameNode(value="User")),
                    arguments=[],
                    directives=[],
                )
            ],
            directives=[],
            interfaces=[],
        ),
        ObjectTypeDefinitionNode(
            name=NameNode(value="User"),
            fields=[
                FieldDefinitionNode(
                    name=NameNode(value="id"),
                    type=NamedTypeNode(name=NameNode(value="ID")),
                    arguments=[],
                    directives=[],
                ),
                FieldDefinitionNode(
                    name=NameNode(value="name"),
                    type=NamedTypeNode(name=NameNode(value="String")),
                    arguments=[],
                    directives=[],
                ),
            ],
            directives=[],
            interfaces=[],
        ),
    ]
)
print("a", document, print_ast(document))
