import abc
import discord


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


class Guild(abc.ABC):
    pass


Guild.register(discord.Guild)
Guild.register(DataStorageGuild)

print(issubclass(discord.Guild, Guild))
print(issubclass(DataStorageGuild, Guild))
