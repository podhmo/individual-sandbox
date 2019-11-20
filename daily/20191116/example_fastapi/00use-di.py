from pprint import pprint
from fastapi.routing import Request
from fastapi.dependencies.utils import get_dependant
from fastapi.dependencies.utils import get_typed_signature


def f(n: int, req: Request):
    pass


print(get_dependant(path="/f", call=f))
pprint(vars(get_dependant(path="/f", call=f)))
print(get_typed_signature(f))

# <fastapi.dependencies.models.Dependant object at 0x7f3e3f1ec410>
# {'background_tasks_param_name': None,
#  'body_params': [],
#  'cache_key': (<function f at 0x7f3e3f1c58c0>, ()),
#  'call': <function f at 0x7f3e3f1c58c0>,
#  'cookie_params': [],
#  'dependencies': [],
#  'header_params': [],
#  'name': None,
#  'path': '/f',
#  'path_params': [],
#  'query_params': [],
#  'request_param_name': None,
#  'response_param_name': None,
#  'security_requirements': [],
#  'security_scopes': None,
#  'security_scopes_param_name': None,
#  'use_cache': True,
#  'websocket_param_name': None}
