from __future__ import annotations
import asyncio
import os
import typing as t
import sys
from handofcats import as_command
from slack import WebClient, RTMClient


class Handler:
    def __init__(self, *, prompt, input):
        self.prompt = prompt
        self.input = input

    async def run(self):
        import re

        prompt = self.prompt
        get_input = self.input.get_input

        await prompt.message(
            "Please visit this URL to authorize this application: <https://example.net>"
        )
        rx = re.compile(r"[0-9a-f]{4}")
        await prompt.prompt(
            f"Enter the authorization code: (validation={rx.pattern})"
        )  # salt?
        life = 3

        result = None
        for i in range(life):
            text = await get_input()
            if rx.search(text) is not None:
                result = text
                break
            await prompt.message("invalid text")

        if result is not None:
            await prompt.message(f"ok: {result}", private=False)
        else:
            await prompt.message("giveup", private=False)


class ConsolePrompt:
    def __init__(self, *, writer: t.Optional[t.IO[str]] = None) -> None:
        self.writer = writer or sys.stderr

    async def prompt(self, text: str) -> None:
        print(text, file=self.writer)

    async def message(self, text: str, *, private: bool = True) -> None:
        print(text, file=self.writer)


class ConsoleInput:
    def __init__(self, *, reader: t.Optional[t.IO[str]] = None) -> None:
        self.reader = reader or sys.stdin

    async def get_input(self) -> str:
        return self.reader.readline().strip()


class SlackPrompt:
    def __init__(
        self,
        *,
        client: WebClient,
        channel: str = "#random",
        user: t.Optional[str] = None,
        salt: str,
    ) -> None:
        self.client = client
        self.channel = channel
        self.user_id = user
        self.salt = salt

    async def prompt(self, text: str) -> None:
        return await self.message(f"`{self.salt}:{text}`")

    async def message(self, text: str, *, private: bool = True) -> None:
        payload = {
            "attachments": [{"color": "#2eb886", "text": text, "footer": "Slack API"}],
        }
        if private and self.user_id is not None:
            await self.client.chat_postEphemeral(
                channel=self.channel, user=self.user_id, **payload,
            )
        else:
            await self.client.chat_postMessage(
                channel=self.channel, **payload,
            )


class SlackInput:
    def __init__(self, mbox: Mbox, *, salt: str) -> None:
        self.salt = salt
        self.mbox = mbox

    async def get_input(self) -> str:
        salt = self.salt
        return await self.mbox.register(salt)


class Mbox:
    # todo: timeout
    def __init__(self):
        self.futs: t.Dict[asyncio.Future] = {}

    def register(self, phrase):
        fut = asyncio.Future()
        self.futs[phrase] = fut
        return fut


mbox = Mbox()


# todo: separation
@RTMClient.run_on(event="message")
async def feed(**payload):
    global mbox

    web_client: WebClient = payload["web_client"]
    data = payload.get("data") or {}
    # todo: lock
    if "text" not in data:
        return

    phrase = data["text"].split(":", 1)[0]
    fut = mbox.futs.get(phrase)
    if fut is None:
        return

    input_text = data["text"][len(phrase) :].strip()
    fut.set_result(input_text)
    channel_id = data["channel"]
    user = data["user"]
    await web_client.chat_postEphemeral(
        channel=channel_id, text=f"got {input_text}", user=user,
    )


@RTMClient.run_on(event="message")
async def register(**payload):
    """`$register` start registration"""
    global mbox
    web_client: WebClient = payload["web_client"]
    data = payload.get("data") or {}

    if "text" not in data:
        return

    if not data["text"].startswith("$register"):
        return

    user: str = data["user"]
    channel: str = data["channel"]

    salt = "xyxy"
    h = Handler(
        prompt=SlackPrompt(client=web_client, channel=channel, user=user, salt=salt),
        input=SlackInput(mbox, salt=salt),
    )
    loop = asyncio.get_event_loop()
    print("hmm..")
    # todo: timeout
    loop.create_task(h.run())
    print("..hmm")
    # await h.run()


async def run_bot():
    loop = asyncio.get_event_loop()
    rtm_client = RTMClient(
        token=os.environ["SLACK_API_TOKEN"], run_async=True, loop=loop
    )
    await rtm_client.start()


@as_command
def run():
    asyncio.run(run_bot(), debug=True)
