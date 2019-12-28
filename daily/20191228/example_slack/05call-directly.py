import sys
import os
import argparse
import logging

import dotenv
from slackbot.slackclient import SlackClient
from slackbot.dispatcher import Message
from slackbot.bot import listen_to


class Exit(Exception):
    pass


class Parser(argparse.ArgumentParser):
    def exit(self, status=0, message=None):
        if message:
            self._print_message(message, sys.stderr)
        raise Exit(message)


@listen_to("^ *\$hello")
def hello_send(message: Message):
    import re

    text = message.body["text"]
    parser = Parser()
    parser.add_argument("--name", default="world")
    argv = [x for x in re.split(r"\s+", text.split("$hello", 1)[1]) if x]
    try:
        args = parser.parse_args(argv)
        print(f"parsed: {args}", file=sys.stderr)
        return message.send(f"hello {args.name}")
    except Exit as e:
        texts = []
        if e.args[0]:
            texts.append(e.args[0])
        texts.append(parser.format_help())
        new_line = "\n"
        return message.send(
            f"""\
```
{new_line.join(texts)}
```"""
        )


def fake_message(client: SlackClient, text: str, *, channel: str) -> Message:
    data = {"text": text, "channel": channel}
    return Message(client, data)


logging.basicConfig(level=logging.DEBUG)
dotenv.load_dotenv(verbose=True)
token = os.environ["SLACKBOT_API_TOKEN"]

client = SlackClient(token)
client.rtm_connect()

# message = fake_message(client, "$hello")
# hello_send(message)
res = client.webapi.channels.list(exclude_archived=True)
channels = {ch["name"]: ch["id"] for ch in res.body["channels"]}

message = fake_message(client, "$hello --name=world", channel=channels["random"])
print("@@", hello_send(message))
import time

time.sleep(1)
print("end")
