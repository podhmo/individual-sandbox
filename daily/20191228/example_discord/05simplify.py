import sys
import os
from dotenv import load_dotenv
from discord.ext import commands
from monogusa import ignore
from monogusa.chatbot import runtime
from monogusa.cli import runtime as cli_runtime

bot = commands.Bot(command_prefix="$")


def hello(*, name: str = "world") -> None:
    """hello message"""
    print(f"hello {name}")


def ng(*, name: str = "world") -> None:
    """NG"""
    1 / 0


def byebye(*, name: str = "world") -> None:
    """byebye :wave:"""
    print(f"byebye {name}")


DRIVER = None


@bot.command()
@ignore
async def app(ctx: commands.Context):
    global DRIVER

    text = ctx.message.content
    argv = runtime.parse(text, name="$app")

    with runtime.handle() as output_list:
        if DRIVER is None:
            parser = runtime.ExitExceptionParser(prog="$app")
            DRIVER = cli_runtime.AsyncDriver(parser=parser)
            await DRIVER.run(argv, module=sys.modules[__name__], debug=True)
        else:
            await DRIVER._run(argv, debug=True)

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
