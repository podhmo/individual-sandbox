class BaseResolver:
    def __init__(self, stage, data):
        self.stage = stage
        self.data = data

    @property
    def service_name(self):
        return self.data["me"]

    @property
    def prefix(self):
        return self.data["prefix"]

    @property
    def host(self):
        return self.data["host"]

    def resolve_db_url(self, name, port):
        name = "{name}.{host}".format(name=name, host=self.host)
        return self.resolve_url(self.prefix, name, port) + "/" + self.service_name

    def resolve_api_url(self, name, port):
        name = "{name}.{host}".format(name=name, host=self.host)
        return self.resolve_url(self.prefix, name, port) + "/api/v1"


class LocalResolver(BaseResolver):
    def resolve_url(self, prefix, name, port):
        return "http://localhost:{port}".format(port=port)


class InfraResolver(BaseResolver):
    def resolve_url(self, prefix, name, port):
        return "http://{prefix}{name}".format(prefix=prefix, name=name)


def get_resolver(stage, data):
    if stage == "local":
        return LocalResolver(stage, data)
    else:
        return InfraResolver(stage, data)
