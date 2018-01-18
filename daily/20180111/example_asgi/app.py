from pyramid.config import Configurator
from pyramid.response import Response
from uvitools.adapters import ASGIAdapter


def hello_world(request):
    return Response('Hello %(name)s!' % request.matchdict)


config = Configurator()
config.add_route('hello', '/hello/{name}')
config.add_view(hello_world, route_name='hello')
app = config.make_wsgi_app()
asgi = ASGIAdapter(app)
