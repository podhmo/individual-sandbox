import asyncio
import os
from slack import WebClient

client = WebClient(token=os.environ["SLACK_API_TOKEN"], run_async=True)

text = """\
# ここにタイトル

なにか文章を書いてみる。

## ここはサブタイトル

箇条書きなども機能する?

- 1つ目
- 2つ目
- 3つ目

コードとかはどうだろう？

```py
def hello() -> str:
    return "hello"
```
"""
future = client.chat_postMessage(
    channel=os.environ.get("SLACK_DEFAULT_CHANNEL", "#random"),
    text=text,
    mrkdown=True,
#    parse="full",  # default is none
)

loop = asyncio.get_event_loop()
# run_until_complete returns the Future's result, or raise its exception.
response = loop.run_until_complete(future)
print(response)
# assert response["message"]["text"] == "Hello world!"
