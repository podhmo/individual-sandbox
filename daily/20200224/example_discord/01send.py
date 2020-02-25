import os
import asyncio
from discord.http import HTTPClient
from discord.enums import ChannelType
from handofcats import as_command


@as_command
def run():
    async def run(token):
        client = HTTPClient()
        await client.static_login(token.strip(), bot=True)

        channels = {}
        response = await client.get_guilds(limit=100)
        for guild in response:
            # todo: filter
            for ch in await client.get_all_guild_channels(guild["id"]):
                if ch["type"] == ChannelType.text.value:
                    channels[ch["name"]] = ch["id"]

        channel_id = channels["general"]
        print(await client.send_message(channel_id, "hello"))
        await client.close()

    token = os.environ["DISCORDCLI_API_TOKEN"]
    asyncio.run(run(token))
