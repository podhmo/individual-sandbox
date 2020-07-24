import asyncio
import os
from slack import WebClient

client = WebClient(token=os.environ["SLACK_API_TOKEN"], run_async=True)
# type section, text.type plain_text, text.text "hello world" ?
print(f'**{os.environ.get("SLACK_DEFAULT_CHANNEL", "#random")}**')
attachments = [
    {
        "fallback": "3択クイズが投稿されました",
        "callback_id": "quiz",  # "callback_id"で、コールバックIDを設定
        "text": "夜が明けるとどうなる？",
        "actions": [  # "actions"で、ボタン名とvalueを設定
            {"name": "answer", "text": "日が昇る", "type": "button", "value": "correct"},
            {"name": "answer", "text": "夜が明ける", "type": "button", "value": "incorrect"},
            {
                "name": "answer",
                "text": "メンテが始まる",
                "type": "button",
                "value": "incorrect",
            },
        ],
        "footer": "https://www.dkrk-blog.net/slack/slack_api08",
    }
]

loop = asyncio.get_event_loop()
response = loop.run_until_complete(
    client.chat_postMessage(
        channel=os.environ.get("SLACK_DEFAULT_CHANNEL", "#random"),
        attachments=attachments,
        icon_emoji=":question:",
        username="Quiz",
    )
)
