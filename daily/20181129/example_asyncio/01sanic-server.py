from sanic import Sanic


async def echo(request, ws):
    while True:
        data = await ws.recv()
        print(data)
        if data == "close":
            await ws.close()
        else:
            await ws.send(data + "/answer")


app = Sanic(__name__)
app.add_websocket_route(echo, "/ws")
app.run(host="0.0.0.0", port=8080, debug=True)
