import asyncio
import os
from slack import WebClient


async def run():
    loop = asyncio.get_event_loop()
    client = WebClient(token=os.environ["SLACK_API_TOKEN"], run_async=True, loop=loop)

    text = f"""\
    開始
    """
    payload = {
        "attachments": [{"color": "#2eb886", "text": text, "footer": "Slack API"}],
    }

    response = await client.chat_postMessage(
        channel=os.environ.get("SLACK_DEFAULT_CHANNEL", "#random"), **payload,
    )
    await asyncio.sleep(1)

    for i in range(1, 10):
        stars = "*" * i
        text = f"""\
        実行中 {stars}
        """
        payload = {
            "attachments": [{"color": "#2eb886", "text": text, "footer": "Slack API"}],
            "ts": response["ts"],
        }

        response = await client.chat_update(channel=response["channel"], **payload,)
        await asyncio.sleep(0.5)

    text = """\
    完了
    """
    payload = {
        "attachments": [{"color": "#3AA3E3", "text": text, "footer": "Slack API",}],
        "ts": response["ts"],
    }
    response = await client.chat_update(channel=response["channel"], **payload,)
    print(response)


asyncio.run(run())
