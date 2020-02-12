import typing as t
import typing_extensions as tx
import dataclasses
import re

# https://github.com/lins05/slackbot#create-plugins
@dataclasses.dataclass
class Message:
    body: str

    # https://github.com/lins05/slackbot/blob/develop/slackbot/dispatcher.py#L190
    def reply(self, msg: str, *, in_thread: bool = False) -> None:
        print("@reply", msg, in_thread)

    def react(self, msg: str) -> None:
        print("@react", msg)

    def send(self, msg: str) -> None:
        print("@send", msg)


class Handler(tx.Protocol):
    def __call__(self, msg: Message, *text: str) -> None:
        ...


class FakeApp:
    def __init__(self) -> None:
        self.handlers: t.List[t.Tuple[t.Pattern[str], Handler]] = []

    def received_message(self, text: str) -> None:
        message = Message(body=text)
        for rx, h in self.handlers:
            m = rx.search(message.body)
            if m is not None:
                h(message, *m.groups())

    def register(self, pattern: str, option: t.Optional[t.Any] = None):
        args = []
        if option is not None:
            args.append(option)
        rx = re.compile(pattern, *args)

        def _register(fn: t.Callable[[Message], None]):
            self.handlers.append((rx, fn))
            return fn

        return _register


app = FakeApp()
# 手抜き
listen_to = app.register
respond_to = app.register


@respond_to("hi", re.IGNORECASE)
def hi(message):
    message.reply("I can understand hi or HI!")
    # react with thumb up emoji
    message.react("+1")


@listen_to("Can someone help me?")
def help(message):
    # Message is replied to the sender (prefixed with @user)
    message.reply("Yes, I can!")

    # Message is sent on the channel
    message.send("I can help everybody!")

    # Start a thread on the original message
    message.reply("Here's a threaded reply", in_thread=True)


@respond_to("Give me (.*)")
def giveme(message, something):
    message.reply("Here is {}".format(something))


if __name__ == "__main__":
    app.received_message("hi")
    app.received_message("HI")
    app.received_message("ih")

    print("-")
    app.received_message("Can someone help me?")

    print("-")
    app.received_message("Give me money")
