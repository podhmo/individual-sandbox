from starlette.applications import Starlette
from starlette.responses import HTMLResponse
from starlette.websockets import WebSocket
from starlette.routing import Route, WebSocketRoute
from starlette.endpoints import WebSocketEndpoint


class Hello(WebSocketEndpoint):
    encoding = "text"

    async def on_connect(self, ws):
        await ws.accept()

    async def on_receive(self, ws, data):
        await ws.send_text(f'{"message": "hello {data}"}')

    # async def on_disconnect(self, ws, data):
    #     print("hmm", data)
    #     print("byebye")


# asgi app
# async def hello(scope, receive, send):
#     assert scope["type"] == "websocket"
#     ws = WebSocket(scope=scope, receive=receive, send=send)
#     await ws.accept()
#     await ws.send_text("hello world")
#     await ws.close()


async def index(request):
    html = """\
<!DOCTPE html>
<html>
<body>
hello
</body>
<script type="text/javascript">
const ws = new WebSocket(`ws://${window.location.host}/ws`);
ws.onmessage = (e) => {
  const data = JSON.parse(e.data);
  console.log(data);
};
ws.onclose = (e) => {
  console.error(e)
};
</script>
</html>
"""
    return HTMLResponse(html)


app = Starlette(debug=True, routes=[Route("/", index), WebSocketRoute("/ws", Hello)])


def test_app():
    from starlette.testclient import TestClient

    c = TestClient(app)
    with c.websocket_connect("/ws") as ws:
        data = ws.receive_text()
        print(data)


if __name__ == "__main__":
    test_app()
