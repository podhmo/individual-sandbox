import os
from dotenv import load_dotenv
from discord.ext import commands

bot = commands.Bot(command_prefix="$")


@bot.command()
async def hello(ctx: commands.Context, *args, **kwargs):
    print(f"@@ {args=} {kwargs=}")
    print(f"@@@@ content={ctx.message.content!r}")
    await ctx.send("hello")


load_dotenv(verbose=True)
token = os.environ["DISCORDBOT_API_TOKEN"]
bot.run(token)
