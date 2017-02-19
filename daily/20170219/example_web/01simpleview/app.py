import venusian
from wsgiref.simple_server import make_server
from pyramid.config import Configurator, PHASE1_CONFIG
from pyramid.response import Response


def add_simple_view(config, view, path):
    def callback():
        route_name = view.__qualname__
        config.add_route(route_name, path)
        config.add_view(view, route_name=route_name)
    discriminator = ('add_simple_view', path)
    config.action(discriminator, callback, order=PHASE1_CONFIG)


class simple_view(object):
    def __init__(self, path):
        self.path = path

    def register(self, scanner, name, wrapped):
        scanner.config.add_simple_view(wrapped, self.path)

    def __call__(self, wrapped):
        venusian.attach(wrapped, self.register)
        return wrapped


@simple_view("/hello/{name}")
def hello_world(request):
    return Response('Hello %(name)s!' % request.matchdict)


if __name__ == '__main__':
    config = Configurator()
    config.add_directive("add_simple_view", add_simple_view)
    config.scan(__name__)
    app = config.make_wsgi_app()
    server = make_server('0.0.0.0', 8080, app)
    server.serve_forever()
