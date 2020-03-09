import os
import discord
from discord.message import Message


class Reaction:
    def __init__(self, name):
        self.name = name
        self.fns = []

    @property
    def __name__(self):
        return self.name

    def register(self, fn):
        self.fns.append(fn)
        return fn

    async def __call__(self, *args, **kwargs):
        for fn in self.fns:
            await fn(*args, **kwargs)


class ReactionRouter:
    def __init__(self, setter):
        self.reactions = {}
        self.setter = setter

    def register(self, fn):
        name = fn.__name__
        reaction = self.reactions.get(name)

        if reaction is None:
            reaction = self.reactions[name] = Reaction(name)

            # for client.event
            # discord.pyがawaitable functionしか許してないみたい
            # __call__がawaitableなオブジェクトを許しても良いような気がする)
            async def reaction_function(*args, **kwargs):
                return await reaction(*args, **kwargs)

            # client.eventが `__name__` を見るようなのでhack。↑が解决すれば不要なはず。。
            reaction_function.__name__ = name
            self.setter(reaction_function)

        return reaction.register(fn)


client = discord.Client()
reaction_router = ReactionRouter(client.event)
event = reaction_router.register


@event
async def on_ready():
    print("We have logged in as {0.user}".format(client))


@event
async def on_ready():  # noqa F811
    print("o_o")


@event
async def on_message(message: Message):
    if message.author == client.user:
        return

    if message.content.startswith("$hello"):
        await message.channel.send("Hello!")


@event
async def on_message(message: Message):  # noqa F811
    if message.author == client.user:
        return

    if message.content.startswith("$byebye"):
        await message.channel.send(f"Byebye, {message.author.display_name}")


# import logging
# logging.basicConfig(level=logging.DEBUG)
token = os.environ["DISCORD_API_TOKEN"]
client.run(token)
