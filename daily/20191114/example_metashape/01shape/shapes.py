from metashape.outputs.graphql import scan
from metashape.runtime import get_walker


class Greeting:
    message: str


ctx = scan(get_walker(aggressive=True))
ctx.dumper.dump(ctx, ctx.config.option.output)  # xxx
print(ctx.result)

# TODO:
"""
    type Query {
        hello: Greeting!
    }
    type Greeting {
        message: String!
    }
"""
