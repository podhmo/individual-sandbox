import typing as t
import pathlib
import json
import graphql as g
from handofcats import as_command
from dictknife import loading

schema = g.build_schema(
    """
type PageInfo {
  hasNextPage: Boolean!
  hasPreviousPage: Boolean!
  startCursor: String
  endCursor: String
}

########################################

interface Node {
  id: ID!
}

########################################

type Card implements Node {
  id: ID!
  title: String!
  content: String
}

type CardConnection {
  edges: [CardEdge]
  pageInfo: PageInfo!
}

type CardEdge {
  cursor: String!
  node: Card
}

type Query {
  cards(
  first: Int,
  after: String,
  last: Int,
  before: String,
): CardConnection
}
"""
)

Card = t.Dict[str, t.Any]


class Root:
    def __init__(self, data):
        self.data = data

    def cards(
        self,
        info,
        *,
        first: t.Optional[int] = None,
        after: t.Optional[str] = None,
        last: t.Optional[int] = None,
        before: t.Optional[str] = None,
    ) -> t.List[Card]:
        return {
            "edges": [
                {
                    "node": {"id": f"cards:{row['id']}", "title": row["title"]},
                    "cursor": f"cards:{row['id']}",
                }
                for row in self.data[:first]
            ],
            "pageInfo": {
                "hasNextPage": bool(self.data[:first]),
                "startCursor": "cards:1",
            },
        }


@as_command
def run():
    with open(pathlib.Path(__file__).parent / ("./cards.json")) as rf:
        cards = json.load(rf)

    # todo: id with prefix
    q = """
query {
  cards(first: 2) {
    edges {
      node { id, title }
    }
    pageInfo {
      startCursor, hasNextPage
    }
  }
}
    """
    result = g.graphql_sync(schema, q, Root(cards))
    print(result.errors)
    loading.dumpfile(result.data)
