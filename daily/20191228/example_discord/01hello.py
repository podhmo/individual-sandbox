import os
import argparse
import contextlib
import sys
from io import StringIO
from dotenv import load_dotenv
from handofcats.injector import Injector
from discord.ext import commands


class Exit(Exception):
    pass


class Parser(argparse.ArgumentParser):
    def exit(self, status=0, message=None):
        if message:
            # TODO: debug?
            self._print_message(message, sys.stderr)
            raise Exit(message)


def _hello(*, name: str = "world"):
    print(f"hello {name}")


bot = commands.Bot(command_prefix="$")


@bot.command()
async def hello(ctx: commands.Context, *argv: str, **kwargs):
    parser = Parser(prog="$hello")
    Injector(_hello).inject(parser)

    output_list = []
    try:
        with contextlib.redirect_stdout(StringIO()) as o:
            args = parser.parse_args(argv)
            params = vars(args).copy()
            _hello(**params)
    except Exit as e:
        output_list.append(str(e))

    output = o.getvalue()
    if output.strip():
        output_list.append(output)
    new_line = "\n"
    await ctx.send(
        f"""\
```
{new_line.join(output_list).strip(new_line)}
```"""
    )


load_dotenv(verbose=True)
token = os.environ["DISCORDBOT_API_TOKEN"]
bot.run(token)
