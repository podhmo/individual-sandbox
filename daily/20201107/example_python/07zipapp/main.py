from starlette.applications import Starlette
from starlette.responses import PlainTextResponse
from starlette.routing import Route, Mount, WebSocketRoute


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


def main():
    print("@", app)


if __name__ == "__main__":
    main()
