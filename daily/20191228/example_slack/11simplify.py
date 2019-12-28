import sys
import os
from slackbot.bot import Bot, listen_to, settings
from slackbot.dispatcher import Message
from monogusa import ignore
from monogusa.cli import runtime as cli_runtime
from monogusa.chatbot import runtime


def hello(*, name: str = "world") -> None:
    """hello message"""
    print(f"hello {name}")


def byebye(*, name: str = "world") -> None:
    """byebye :wave:"""
    print(f"byebye {name}")


def ng(*, name: str = "world") -> None:
    """:warning: NG"""
    1 / 0


DRIVER = None


@ignore
@listen_to(r"^ *\$app")
def app(message: Message):
    global DRIVER
    text = message.body["text"]
    argv = runtime.parse(text, name="$app")

    with runtime.handle() as output_list:
        if DRIVER is None:
            parser = runtime.ExitExceptionParser(prog="$app")
            DRIVER = cli_runtime.Driver(parser=parser)
            DRIVER.run(argv, module=sys.modules[__name__], debug=True)
        else:
            DRIVER._run(argv, debug=True)

    new_line = "\n"
    if output_list:
        message.reply(
            f"""\
```
{new_line.join(output_list).strip(new_line)}
```""",
            in_thread=True,
        )


@ignore
def main():
    import dotenv
    import logging

    logging.basicConfig(level=logging.DEBUG)

    # update API_TOKEN
    dotenv.load_dotenv()
    settings.API_TOKEN = os.environ["SLACKBOT_API_TOKEN"]

    bot = Bot()
    bot.run()


if __name__ == "__main__":
    main()
