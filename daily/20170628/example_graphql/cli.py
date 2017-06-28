import urllib.request as r
import json


class GraphQLClient:
    def __init__(self, endpoint):
        self.endpoint = endpoint
        self.token = None

    def execute(self, query, variables=None):
        return self._send(query, variables)

    def inject_token(self, token):
        self.token = token

    def _send(self, query, variables):
        data = {'query': query, 'variables': variables}
        headers = {'Accept': 'application/json', 'Content-Type': 'application/json'}

        if self.token is not None:
            headers['Authorization'] = 'Bearer %s' % self.token

        req = r.Request(self.endpoint, json.dumps(data).encode("utf-8"), headers)

        return r.urlopen(req)


if __name__ == "__main__":
    client = GraphQLClient("http://localhost:5000/graphql")
    q = """
{
  allEmployees {
    edges {
      node {
        id
        name
        hiredOn
        role {
          id
          name
        }
        department {
          id
          name
        }
      }
    }
  }
}
"""
    response = client.execute(q)
    print(json.dumps(json.loads(response.read().decode("utf-8")), indent=2))
