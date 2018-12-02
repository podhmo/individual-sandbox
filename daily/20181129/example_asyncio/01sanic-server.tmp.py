from sanic import Sanic, Blueprint

bp = Blueprint("ws")


@bp.websocket('/ws')
async def echo(request, ws):
    while True:
        data = await ws.recv()
        print(data)
        await ws.send(data + "/answer")


app = Sanic(__name__)
app.blueprint(bp)
app.run(host="0.0.0.0", port=8080, debug=True)
