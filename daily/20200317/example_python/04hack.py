import discord
from typing import NewType, Union


class DataStorageGuild:
    def __init__(self, guild: discord.Guild):
        self._guild = guild

    def __getattr__(self, name):
        return getattr(self._guild, name)

    def _get_channel(self, id):
        for c in self._guild.channels:
            if c.name == str(id):
                return c

    async def read(self, id):
        channel = self._get_channel(id)
        message = channel.last_message or await channel.fetch_message(
            self.channel.last_message_id
        )
        return message.content

    async def write(self, id, content):
        channel = self._get_channel(id)
        await channel.send(content)


DataStorageGuild = NewType(
    "DataStorageGuild", Union[discord.Guild, DataStorageGuild]
)

client = discord.Client()
GUILD_ID = 354657642345367
db_guild = client.get_guild(GUILD_ID)
storage = DataStorageGuild(DataStorageGuild(db_guild))
print(storage.write("data-id", "new_data"))
print(storage.read("data-id"))
