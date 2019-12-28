from slackbot.bot import Bot
from slackbot.bot import listen_to
from slackbot.dispatcher import Message


def hello_send(message: Message):
    text = message.body["text"]
    message.send(f"!! {text} !!")


hello_send = listen_to("^ *\$hmm")(hello_send)


def main():
    bot = Bot()
    bot.run()


if __name__ == "__main__":
    import dotenv
    import logging
    from importlib import reload
    import slackbot.settings
    import slackbot.bot

    logging.basicConfig(level=logging.DEBUG)
    # overwrite with dotenv
    dotenv.load_dotenv(verbose=True)
    slackbot.bot.settings = reload(slackbot.settings)
    slackbot.settings.PLUGINS = []
    main()
