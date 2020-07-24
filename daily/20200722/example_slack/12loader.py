import typing as t
import asyncio
import pathlib
import sys
from handofcats import as_command
from gspread.auth import (
    InstalledAppFlow,
    load_credentials,
    store_credentials,
)
from gspread.auth import DEFAULT_SCOPES


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
        await self.prompt.message(flow._DEFAULT_AUTH_PROMPT_MESSAGE.format(url=auth_url))
        await self.prompt.prompt(flow._DEFAULT_AUTH_CODE_MESSAGE)
        life = 3

        result = None
        for i in range(life):
            code = await get_input()
            try:
                flow.fetch_token(code=code)
                break
            except Exception as e:
                await prompt.message(f"invalid text {e}")

        if result is not None:
            await prompt.message(f"ok: {result}", private=False)
            creds = flow.credentials
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


@as_command
def run(*, auth_file: str) -> None:
    h = Handler(prompt=ConsolePrompt(), input=ConsoleInput())
    coro = h.run(authorized_user_filename=auth_file)
    creds = asyncio.run(coro, debug=True)
    print(type(creds), creds)
