import aiohttp.web


async def handler(request):
    ws = aiohttp.web.WebSocketResponse()
    await ws.prepare(request)

    async for msg in ws:
        print(msg)
        if msg.type == aiohttp.WSMsgType.TEXT:
            print(msg.data)
            if msg.data == 'close':
                await ws.close()
            else:
                await ws.send_str(msg.data + '/answer')

    print('Websocket connection closed')
    return ws


def main():
    app = aiohttp.web.Application()
    app.router.add_route('GET', '/ws', handler)
    aiohttp.web.run_app(app, host="0.0.0.0", port=8080)


if __name__ == '__main__':
    main()
