from starlette.applications import Starlette
from starlette.responses import HTMLResponse
from starlette.routing import Route, WebSocketRoute


async def hello(ws):
    await ws.accept()
    await ws.send_text("hello world")
    await ws.close()


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
  const data = e.data;
  console.log(data);
};
ws.onclose = (e) => {
  console.error(e)
};
</script>
</html>
"""
    return HTMLResponse(html)


app = Starlette(debug=True, routes=[Route("/", index), WebSocketRoute("/ws", hello)])


def test_app():
    from starlette.testclient import TestClient

    c = TestClient(app)
    with c.websocket_connect("/ws") as ws:
        data = ws.receive_text()
        print(data)


if __name__ == "__main__":
    test_app()
