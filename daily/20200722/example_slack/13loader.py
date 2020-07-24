from __future__ import annotations
import typing as t
import asyncio
import pathlib
import sys
import os
import random
import string
from handofcats import as_subcommand
from gspread.auth import (
    InstalledAppFlow,
    load_credentials,
    store_credentials,
)
from gspread.auth import DEFAULT_SCOPES
from slack import WebClient, RTMClient


class Handler:
    def __init__(self, *, prompt, input):
        self.prompt = prompt
        self.input = input

    async def run(
        self,
        credential_filename: str = "~/.config/sheetconf/credentials.json",
        authorized_user_filename: str = ".auth.out",
        scopes: t.Optional[t.List[str]] = DEFAULT_SCOPES,
    ):
        prompt = self.prompt
        get_input = self.input.get_input

        authorized_user_path = pathlib.Path(authorized_user_filename).expanduser()
        credential_path = pathlib.Path(credential_filename).expanduser()
        if not authorized_user_path.parent.exists():
            authorized_user_path.parent.mkdir(parents=True)
        if not credential_path.parent.exists():
            credential_path.parent.mkdir(parents=True)

        creds = load_credentials(filename=str(authorized_user_path))

        if creds:
            return creds

        try:
            flow = InstalledAppFlow.from_client_secrets_file(
                str(credential_path), scopes
            )
        except FileNotFoundError:
            raise  # xxx

        kwargs = {}
        kwargs.setdefault("prompt", "consent")

        flow.redirect_uri = flow._OOB_REDIRECT_URI
        auth_url, _ = flow.authorization_url(**kwargs)
        await self.prompt.message(
            flow._DEFAULT_AUTH_PROMPT_MESSAGE.format(url=auth_url)
        )
        await self.prompt.prompt(flow._DEFAULT_AUTH_CODE_MESSAGE)
        life = 3

        result = None
        for i in range(life):
            code = await get_input()
            try:
                flow.fetch_token(code=code)
                result = flow.credentials
                break
            except Exception as e:
                await prompt.message(f"invalid text {e}")

        if result is not None:
            await prompt.message(f"ok: {result}", private=False)
            store_credentials(creds, filename=str(authorized_user_path))
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
        sep: str = ":",
    ) -> None:
        self.client = client
        self.channel = channel
        self.user_id = user
        self.salt = salt
        self.sep = sep

    async def prompt(self, text: str) -> None:
        return await self.message(f"{text}`{self.salt}{self.sep}<code>`")

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

        def _cleanup(fut):
            self.futs.pop(phrase)

        fut.add_done_callback(_cleanup)
        self.futs[phrase] = fut
        return fut


mbox = Mbox()


# todo: separation
@RTMClient.run_on(event="message")
async def feed(*, sep: str = ":", **payload):
    global mbox

    web_client: WebClient = payload["web_client"]
    data = payload.get("data") or {}
    # todo: lock
    if "text" not in data:
        return

    phrase = data["text"].split(sep, 1)[0]
    fut = mbox.futs.get(phrase)
    if fut is None:
        return

    input_text = data["text"][len(phrase) + len(sep) :].strip()
    fut.set_result(input_text)
    channel_id = data["channel"]
    user = data["user"]
    await web_client.chat_postEphemeral(
        channel=channel_id, text=f"got {input_text}", user=user,
    )


@RTMClient.run_on(event="message")
async def auth(**payload):
    """`$auth` start registration"""
    global mbox
    web_client: WebClient = payload["web_client"]
    data = payload.get("data") or {}

    if "text" not in data:
        return

    if not data["text"].startswith("$auth"):
        return

    user: str = data["user"]
    channel: str = data["channel"]

    salt = "".join(
        random.SystemRandom().choice(string.ascii_uppercase + string.digits)
        for _ in range(5)
    )
    h = Handler(
        prompt=SlackPrompt(client=web_client, channel=channel, user=user, salt=salt),
        input=SlackInput(mbox, salt=salt),
    )
    loop = asyncio.get_event_loop()
    print("hmm..")
    # todo: timeout
    loop.create_task(h.run())  # spawn
    print("..hmm")
    # await h.run()


@as_subcommand
def console(*, auth_file: str) -> None:
    h = Handler(prompt=ConsolePrompt(), input=ConsoleInput())
    coro = h.run(authorized_user_filename=auth_file)
    creds = asyncio.run(coro, debug=True)
    print(type(creds), creds)


@as_subcommand
def bot(*, auth_file: str) -> None:
    async def run_bot():
        loop = asyncio.get_event_loop()
        rtm_client = RTMClient(
            token=os.environ["SLACK_API_TOKEN"], run_async=True, loop=loop
        )
        await rtm_client.start()

    asyncio.run(run_bot(), debug=True)


as_subcommand.run()
