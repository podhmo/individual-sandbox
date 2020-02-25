import sys
import os
import discord
from discord import channel
from handofcats import as_command


@as_command
def run():

    client = discord.Client()

    channels = None

    def get_channel(name: str) -> str:
        nonlocal channels
        if channels is None:
            channels = {}
            for ch in list(client.get_all_channels()):
                if isinstance(ch, channel.TextChannel):
                    channels[ch.name] = ch.id
        return channels[name]

    async def my_background_task():
        await client.wait_until_ready()

        channel_id = get_channel("general")
        print("send to ", channel_id, file=sys.stderr)
        await client.http.send_message(channel_id, "hello world")

        def on_stop():
            client.loop.stop()

        client.loop.call_soon(on_stop)

    client.loop.create_task(my_background_task())
    token = os.environ["DISCORDCLI_API_TOKEN"]
    client.run(token)
