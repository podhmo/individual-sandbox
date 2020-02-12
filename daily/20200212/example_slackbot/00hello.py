import re
from slackbot.bot import Bot
from slackbot.bot import listen_to, respond_to


@listen_to("ho .*", re.IGNORECASE)
@respond_to("ho .*", re.IGNORECASE)
def ho(message):
    message.reply("I can understand hi or HI!")
    # react with thumb up emoji
    message.react("+1")


def main():
    bot = Bot()
    bot.run()


if __name__ == "__main__":
    main()
