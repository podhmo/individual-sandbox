from deps import setup, GroupedDag  # NOQA
dag = GroupedDag()  # NOQA
setup(hook=dag.add)  # NOQA
from wsgiref.simple_server import make_server
from pyramid.config import Configurator
from pyramid.response import Response


def hello_world(request):
    return Response('Hello %(name)s!' % request.matchdict)


def main():
    config = Configurator()
    config.add_route('hello', '/hello/{name}')
    config.add_view(hello_world, route_name='hello')
    app = config.make_wsgi_app()
    server = make_server('0.0.0.0', 8080, app)
    server.serve_forever()


print(dag.to_dot())
# if __name__ == '__main__':
#     main()
