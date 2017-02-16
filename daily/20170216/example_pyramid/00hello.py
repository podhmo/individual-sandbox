import io
import traceback
from wsgiref.simple_server import make_server
from pyramid.config import Configurator


def hello_world(request):
    return {"message": "hello world"}


def exc_view(request):
    tbio = io.StringIO()
    traceback.print_exception(*request.exc_info, file=tbio)
    return {"message": str(request.exception), "traceback": tbio.getvalue()}


def zero_division(request):
    1 / 0

if __name__ == '__main__':
    config = Configurator()
    config.add_route('hello', '/')
    config.add_route('500', '/500')
    config.add_view(hello_world, route_name='hello', renderer="json")
    config.add_view(zero_division, route_name='500', renderer="json")

    config.add_view(view=exc_view, context=Exception, renderer="json")
    app = config.make_wsgi_app()
    server = make_server('0.0.0.0', 8080, app)
    server.handle_request()
