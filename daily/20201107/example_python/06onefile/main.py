import pathlib
import sys
from importlib.machinery import ModuleSpec


class OnefileImporter:  # importer = finder + loader
    def __init__(self, filename):
        self.filename = filename
        self._code_map = None

    @property
    def code_map(self):
        if self._code_map is not None:
            return self._code_map
        with open(self.filename) as rf:
            import ast

            self._code_map = ast.literal_eval(rf.read())
        return self._code_map

    def find_spec(self, fullname, path=None, target=None):
        code = self.code_map.get(fullname)
        if code is None:
            return None
        loader = self
        spec = ModuleSpec(fullname, loader)
        if fullname in self.code_map["@packages@"]:
            spec.submodule_search_locations = [str(pathlib.Path(__file__).parent)]
        return spec

    def create_module(self, spec):
        return None

    def exec_module(self, module):
        code = self.code_map[module.__name__]
        exec(code, module.__dict__)


sys.meta_path.append(OnefileImporter(pathlib.Path(__file__).parent / "__embed__.py"))

# import xxx.yyy.zzz
from foo.applications import Starlette
from foo.responses import PlainTextResponse
from foo.routing import Route, Mount, WebSocketRoute


def homepage(request):
    return PlainTextResponse("Hello, world!")


def user_me(request):
    username = "John Doe"
    return PlainTextResponse("Hello, %s!" % username)


def user(request):
    username = request.path_params["username"]
    return PlainTextResponse("Hello, %s!" % username)


async def websocket_endpoint(websocket):
    await websocket.accept()
    await websocket.send_text("Hello, websocket!")
    await websocket.close()


def startup():
    print("Ready to go")


routes = [
    Route("/", homepage),
    Route("/user/me", user_me),
    Route("/user/{username}", user),
    WebSocketRoute("/ws", websocket_endpoint),
]

app = Starlette(debug=True, routes=routes, on_startup=[startup])
print("@", app)
