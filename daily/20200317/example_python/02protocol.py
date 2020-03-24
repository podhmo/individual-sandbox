import typing as t
import typing_extensions as tx
import discord


@tx.runtime_checkable
class GuildProtocol(tx.Protocol):
    async def read(self, id: t.Any) -> t.Awaitable[t.Any]:
        ...

    async def write(self, id: t.Any, content: t.Any) -> t.Awaitable[None]:
        ...


class DataStorageGuild:
    def __init__(self, guild: discord.Guild) -> None:
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


def main():
    print(issubclass(DataStorageGuild, GuildProtocol))
    print(issubclass(discord.Guild, GuildProtocol))
    g: GuildProtocol = (discord.Guild({}, {}))
    dsg: GuildProtocol = DataStorageGuild(discord.Guild({}, {}))
