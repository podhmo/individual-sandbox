import re
from slackbot.bot import Bot
from slackbot.bot import respond_to
from slackbot.bot import listen_to


@respond_to("hello$", re.IGNORECASE)
def hello_reply(message):
    message.reply("hello sender!")


@listen_to("hello$")
def hello_send(message):
    message.send("hello channel!")


@listen_to("help$")
def help_send(message):
    message.send(message.docs_reply())


def main():
    bot = Bot()
    bot.run()


if __name__ == "__main__":
    import dotenv
    import os
    import logging
    from slackbot.bot import settings

    logging.basicConfig(level=logging.DEBUG)
    dotenv.load_dotenv(verbose=True)
    settings.API_TOKEN = os.environ["SLACKBOT_API_TOKEN"]
    main()
