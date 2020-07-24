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
payload = {
    "text": "<https://hatenablog.com>",
    "unfurl_links": True,
    "blocks": [
        {
            "type": "section",
            "text": {"type": "mrkdwn", "text": "Qiita: <https://www.qiita.com>"},
        }
    ],
    "attachments": [
        {
            "color": "#FD8F07",
            "blocks": [
                {
                    "type": "section",
                    "text": {"type": "mrkdwn", "text": "Amebaブログ <https://ameblo.jp/>"},
                }
            ],
        }
    ],
}
response = loop.run_until_complete(
    client.chat_postMessage(
        channel=os.environ.get("SLACK_DEFAULT_CHANNEL", "#random"), **payload,
    )
)
