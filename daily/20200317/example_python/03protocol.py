import typing as t
import typing_extensions as tx
import discord


@tx.runtime_checkable
class GuildProtocol(tx.Protocol):
    async def read(self, id: t.Any) -> t.Any:
        ...

    async def write(self, id: t.Any, content: t.Any) -> None:
        ...


class DataStorageGuild:
    def __init__(self, guild: discord.Guild) -> None:
        self._guild = guild

    def __getattr__(self, name: str) -> t.Any:
        return getattr(self._guild, name)

    def _get_channel(self, id: t.Any) -> t.Any:
        for c in self._guild.channels:
            if c.name == str(id):
                return c

    async def read(self, id: t.Any) -> t.Any:
        channel = self._get_channel(id)
        message = channel.last_message or await channel.fetch_message(
            self.channel.last_message_id
        )
        return message.content

    async def write(self, id: t.Any, content: t.Any) -> t.Any:
        channel = self._get_channel(id)
        await channel.send(content)


def use(g: GuildProtocol) -> None:
    print(type(g))


def main() -> None:
    print(issubclass(DataStorageGuild, GuildProtocol))
    print(issubclass(discord.Guild, GuildProtocol))
    use(discord.Guild(data={"id": 0}, state={}))
    use(DataStorageGuild(discord.Guild(data={"id": 0}, state={})))


main()
