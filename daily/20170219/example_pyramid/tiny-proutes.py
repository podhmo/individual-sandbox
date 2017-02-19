from pyramid.config import Configurator
from pyramid.scripts.proutes import get_route_data


def hello(request):
    return {"message": "hello"}


def hello2(request):
    return {"message": "post hello"}


def bye(request):
    return {"message": "bye"}


config = Configurator()

config.add_route("hello", "/hello/{name}")
config.add_route("bye", "/bye/{name}")
config.add_view(hello, route_name="hello", request_method="GET")
config.add_view(hello2, route_name="hello", request_method="POST")
config.add_view(bye, route_name="bye")

config.commit()
mapper = config.get_routes_mapper()
routes = mapper.get_routes(include_static=True)
for route in routes:
    route_data = get_route_data(route, config.registry)
    for name, pattern, view, method in route_data:
        print(name, pattern, view, method)

# hello /hello/{name} __main__.hello GET
# hello /hello/{name} __main__.hello2 POST
# bye /bye/{name} __main__.bye *
