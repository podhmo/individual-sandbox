from metashape.outputs.graphql import emit
from metashape.runtime import emit_with


class Greeting:
    message: str


emit_with(emit=emit, aggressive=True)

# TODO:
"""
    type Query {
        hello: Greeting!
    }
    type Greeting {
        message: String!
    }
"""
