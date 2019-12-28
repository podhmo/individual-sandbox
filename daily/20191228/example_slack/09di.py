from __future__ import annotations
import sys
import argparse
import contextlib
import shlex
import logging
from io import StringIO
from slackbot.bot import Bot
from slackbot.bot import listen_to
from slackbot.dispatcher import Message
from monogusa import ignore, component
from monogusa.cli import runtime

logger = logging.getLogger(__name__)


class Exit(Exception):
    pass


class Parser(argparse.ArgumentParser):
    def exit(self, status=0, message=None):
        if message:
            # TODO: debug?
            self._print_message(message, sys.stderr)
        if message is not None:
            raise Exit(message)


def hello(db: DB, *, name: str = "world") -> None:
    """hello message"""
    db.save(f"hello {name}")


def byebye(db: DB, *, name: str = "world") -> None:
    """byebye :wave:"""
    print(f"byebye {name}")


@component
def db() -> DB:
    return DB()


class DB:
    def save(self, msg: str) -> None:
        print(f"DB save: {msg}")


DRIVER = None


@ignore
@listen_to(r"^ *\$app")
def app(message: Message):
    global DRIVER
    text = message.body["text"]
    argv = shlex.split(text.split("$app", 1)[1], posix=True)

    output_list = []
    with contextlib.redirect_stdout(StringIO()) as o:
        with contextlib.redirect_stderr(o):
            try:
                if DRIVER is None:
                    parser = Parser(prog="$app")
                    DRIVER = runtime.Driver(parser=parser)
                    DRIVER.run(argv, module=sys.modules[__name__], debug=True)
                else:
                    DRIVER._run(argv, debug=True)
            except Exit as e:
                logger.debug("exit: %r", e)

    output = o.getvalue()
    if output.strip():
        output_list.append(output)

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
    from importlib import reload
    import slackbot.settings
    import slackbot.bot

    logging.basicConfig(level=logging.DEBUG)
    # overwrite with dotenv
    dotenv.load_dotenv(verbose=True)
    slackbot.bot.settings = reload(slackbot.settings)

    bot = Bot()
    bot.run()


if __name__ == "__main__":
    main()
