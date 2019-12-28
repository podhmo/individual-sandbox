import sys
import argparse
import contextlib
from io import StringIO
from slackbot.bot import Bot
from slackbot.bot import listen_to
from slackbot.dispatcher import Message
from handofcats.injector import Injector


class Exit(Exception):
    pass


class Parser(argparse.ArgumentParser):
    def exit(self, status=0, message=None):
        if message:
            # TODO: debug?
            self._print_message(message, sys.stderr)
        raise Exit(message)


def hello(*, name: str = "world") -> None:
    print(f"hello {name}")


@listen_to("^ *\$hello")
def hello_send(message: Message):
    import shlex

    text = message.body["text"]
    parser = Parser(prog="$hello")
    Injector(hello).inject(parser)

    argv = shlex.split(text.split("$hello", 1)[1], posix=True)
    try:
        args = parser.parse_args(argv)
    except Exit as e:
        texts = []
        if e.args[0]:
            texts.append(e.args[0])
        texts.append(parser.format_help())
        new_line = "\n"
        message.send(
            f"""\
```
{new_line.join(texts).strip(new_line)}
```"""
        )
        return

    params = vars(args).copy()
    with contextlib.redirect_stdout(StringIO()) as o:
        hello(**params)
    output = o.getvalue()
    if output.strip():
        message.send(output)


def main():
    bot = Bot()
    bot.run()


if __name__ == "__main__":
    import dotenv
    import logging
    from importlib import reload
    import slackbot.settings
    import slackbot.bot

    logging.basicConfig(level=logging.DEBUG)
    # overwrite with dotenv
    dotenv.load_dotenv(verbose=True)
    slackbot.bot.settings = reload(slackbot.settings)
    main()
