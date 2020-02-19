from slackbot.bot import listen_to, Bot
from slackbot.dispatcher import Message
import re


@listen_to(".", re.IGNORECASE)
def echo(message: Message):
    message.reply(f"got {message.body.get('text')}")


def main():
    bot = Bot()
    bot.run()


if __name__ == "__main__":
    main()
