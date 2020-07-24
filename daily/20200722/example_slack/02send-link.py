import asyncio
import os
from slack import WebClient

client = WebClient(token=os.environ["SLACK_API_TOKEN"], run_async=True)
# type section, text.type plain_text, text.text "hello world" ?
print(f'**{os.environ.get("SLACK_DEFAULT_CHANNEL", "#random")}**')

text = """
message

https://example.net
<https://example.net>
"""
loop = asyncio.get_event_loop()
response = loop.run_until_complete(
    client.chat_postMessage(
        channel=os.environ.get("SLACK_DEFAULT_CHANNEL", "#random"), text=text,
    )
)
print(response)
response = loop.run_until_complete(
    client.chat_postMessage(
        channel=os.environ.get("SLACK_DEFAULT_CHANNEL", "#random"),
        text=text,
        unfurl_links=True,
    )
)
print(response)
response = loop.run_until_complete(
    client.chat_postMessage(
        channel=os.environ.get("SLACK_DEFAULT_CHANNEL", "#random"),
        text=text,
        unfurl_links=False,
    )
)
print(response)
