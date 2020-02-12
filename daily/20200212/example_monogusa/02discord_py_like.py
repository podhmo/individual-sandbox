import typing as t
import typing_extensions as tx
import asyncio
import dataclasses


class _Channel:
    async def send(self, msg: str) -> None:
        print("@send", msg)


class Channel(tx.Protocol):
    async def send(self, msg: str) -> None:
        ...


@dataclasses.dataclass
class User:
    id: int

    @property
    def mention(self) -> str:
        return f"#<user {self.id}>"


@dataclasses.dataclass
class Message:
    id: int
    author: User
    content: str
    channel: Channel


class MyClient:
    def __init__(self, user: User) -> None:
        self.user = user

    def event(self, fn: t.Callable[..., t.Any]) -> None:
        setattr(self, f"{fn.__name__}", fn)


client = MyClient(User(id=-1))


@client.event
async def on_message(message):
    # we do not want the bot to reply to itself
    if message.author.id == client.user.id:
        return

    if message.content.startswith("!hello"):
        await message.channel.send("Hello {0.author.mention}".format(message))


class FakeApp:
    def __init__(self, client):
        self.client = client

    def received_message(self, msg: str):
        user = User(id=1)
        message = Message(id=0, content=msg, author=user, channel=_Channel())
        asyncio.run(self.client.on_message(message))


app = FakeApp(client)
app.received_message("!hello")
