import asyncio
import os
from slack import WebClient

client = WebClient(token=os.environ["SLACK_API_TOKEN"], run_async=True)
# type section, text.type plain_text, text.text "hello world" ?
print(f'**{os.environ.get("SLACK_DEFAULT_CHANNEL", "#random")}**')
future = client.chat_postMessage(
    channel=os.environ.get("SLACK_DEFAULT_CHANNEL", "#random"),
    text="Hello world!",
    as_user=True,
)

loop = asyncio.get_event_loop()
# run_until_complete returns the Future's result, or raise its exception.
response = loop.run_until_complete(future)
assert response["message"]["text"] == "Hello world!"
