from graphql import extend_schema, parse

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

schema = extend_schema(
    document,
    parse(
        """
    extend type User {
     lastName: String
    }
    """
    ),
)

# https://graphql-core-next.readthedocs.io/en/latest/usage/extension.html
