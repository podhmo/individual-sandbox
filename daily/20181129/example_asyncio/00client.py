import asyncio
import aiohttp


async def main(url):
    async with aiohttp.ClientSession() as session:
        async with session.ws_connect(url) as ws:

            await prompt_and_send(ws)
            async for msg in ws:
                print('Message received from server:', msg)
                await prompt_and_send(ws)

                if msg.type in (aiohttp.WSMsgType.CLOSED, aiohttp.WSMsgType.ERROR):
                    break


async def prompt_and_send(ws):
    new_msg_to_send = input('input msg: ')
    if new_msg_to_send == '':
        raise SystemExit(0)
    await ws.send_str(new_msg_to_send)


loop = asyncio.get_event_loop()
loop.run_until_complete(main("http://localhost:8080/ws"))
