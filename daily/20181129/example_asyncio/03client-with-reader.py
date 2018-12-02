import asyncio
import signal
import sys

import aiohttp


async def start_client(loop):
    async with aiohttp.ClientSession() as session:
        async with session.ws_connect("http://localhost:8080/ws", autoping=False) as ws:

            def stdin_callback():
                line = sys.stdin.buffer.readline().decode('utf-8')
                if not line:
                    loop.stop()
                else:
                    loop.create_task(ws.send_str(line))

            loop.add_reader(sys.stdin.fileno(), stdin_callback)

            async def dispatch():
                while True:
                    msg = await ws.receive()

                    if msg.type == aiohttp.WSMsgType.TEXT:
                        print('Text: ', msg.data.strip())
                    elif msg.type == aiohttp.WSMsgType.BINARY:
                        print('Binary: ', msg.data)
                    elif msg.type == aiohttp.WSMsgType.PING:
                        ws.pong()
                    elif msg.type == aiohttp.WSMsgType.PONG:
                        print('Pong received')
                    else:
                        if msg.type == aiohttp.WSMsgType.CLOSE:
                            await ws.close()
                        elif msg.type == aiohttp.WSMsgType.ERROR:
                            print('Error during receive %s' % ws.exception())
                        elif msg.type == aiohttp.WSMsgType.CLOSED:
                            pass

                        break

            await dispatch()


loop = asyncio.get_event_loop()
loop.add_signal_handler(signal.SIGINT, loop.stop)
loop.create_task(start_client(loop))
loop.run_forever()
