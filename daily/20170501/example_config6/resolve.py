class LocalhostResolver:
    def __init__(self, data):
      self.data = data

    def resolve_endpoint(self, name, port):
        return "http://localhost:{port}/api/v1".format(port=port)


class Resolver:
    def __init__(self, data):
      self.data = data

    def resolve_endpoint(self, name, port):
        return "{name}.{host}/api/v1".format(name=name,host=self.data["host"])


def get_resolver(host, data):
    if host == "localhost":
        return LocalhostResolver(data)
    else:
        return Resolver(data)
