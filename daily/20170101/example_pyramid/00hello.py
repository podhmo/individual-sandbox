from wsgiref.simple_server import make_server
from pyramid.config import Configurator
import os.path


def get_hello_world(request):
    return {"method": "GET", "name": request.matchdict["name"]}


def post_hello_world(request):
    return {"method": "POST", "name": request.matchdict["name"]}


if __name__ == '__main__':
    here = os.path.dirname(os.path.abspath(__file__))
    settings = {"mako.directories": here,
                "pyramid.reload_all": True}
    config = Configurator(settings=settings)

    config.include("pyramid_mako")
    config.add_mako_renderer(".html")

    config.add_route('hello', '/hello/{name}')
    config.add_view(get_hello_world, route_name='hello', renderer="hello.html")
    config.add_view(post_hello_world, route_name='hello', renderer="hello.html", request_method="POST")

    app = config.make_wsgi_app()
    server = make_server('0.0.0.0', 8080, app)
    server.serve_forever()
