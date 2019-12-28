import sys
import os
import argparse
import contextlib
import logging
import shlex
from io import StringIO
from dotenv import load_dotenv
from discord.ext import commands
from monogusa import ignore
from monogusa.cli import runtime

logger = logging.getLogger(__name__)
bot = commands.Bot(command_prefix="$")

# commands not support `$app hello --name="foo"`


class Exit(Exception):
    pass


class Parser(argparse.ArgumentParser):
    def exit(self, status=0, message=None):
        if message:
            # TODO: debug?
            self._print_message(message, sys.stderr)
        if message is not None:
            raise Exit(message)


def hello(*, name: str = "world") -> None:
    """hello message"""
    print(f"hello {name}")


def byebye(*, name: str = "world") -> None:
    """byebye :wave:"""
    print(f"byebye {name}")


DRIVER = None


@bot.command()
@ignore
async def app(ctx: commands.Context):
    global DRIVER

    text = ctx.message.content
    argv = shlex.split(text.split("$app", 1)[1], posix=True)

    output_list = []
    with contextlib.redirect_stdout(StringIO()) as o:
        with contextlib.redirect_stderr(o):
            try:
                if DRIVER is None:
                    parser = Parser(prog="$app")
                    DRIVER = runtime.AsyncDriver(parser=parser)
                    await DRIVER.run(argv, module=sys.modules[__name__], debug=True)
                else:
                    await DRIVER._run(argv, debug=True)
            except Exit as e:
                logger.debug("exit: %r", e)

    output = o.getvalue()
    if output.strip():
        output_list.append(output)

    new_line = "\n"
    if output_list:
        await ctx.send(
            f"""\
```
{new_line.join(output_list).strip(new_line)}
```"""
        )


@ignore
def main():
    load_dotenv(verbose=True)
    token = os.environ["DISCORDBOT_API_TOKEN"]
    bot.run(token)


if __name__ == "__main__":
    main()
