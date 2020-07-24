import asyncio
import os
from slack import WebClient


async def run():
    loop = asyncio.get_event_loop()
    client = WebClient(token=os.environ["SLACK_API_TOKEN"], run_async=True, loop=loop)

    text = """\
    実行中
    """
    payload = {
        "attachments": [{"color": "#2eb886", "text": text, "footer": "Slack API",}],
    }

    response = await client.chat_postMessage(
        channel=os.environ.get("SLACK_DEFAULT_CHANNEL", "#random"), **payload,
    )
    await asyncio.sleep(1)

    text = """\
    完了
    """
    payload = {
        "attachments": [{"color": "#3AA3E3", "text": text, "footer": "Slack API",}],
    }
    payload["ts"] = response["ts"]
    response = await client.chat_update(channel=response["channel"], **payload,)
    print(response)


asyncio.run(run())
