import os
import sys
from dotenv import load_dotenv
from monogusa import ignore
from monogusa.chatbot import discordchat


def hello(*, name: str = "world") -> None:
    """hello message"""
    print(f"hello {name}")


def ng(*, name: str = "world") -> None:
    """NG"""
    1 / 0


def byebye(*, name: str = "world") -> None:
    """byebye :wave:"""
    print(f"byebye {name}")


@ignore
def main():
    import dotenv
    import logging

    logging.basicConfig(level=logging.DEBUG)
    module = sys.modules[__name__]

    dotenv.load_dotenv(verbose=True)
    token = os.environ["DISCORDBOT_API_TOKEN"]
    discordchat.run(token, module=module, name="app")


if __name__ == "__main__":
    main()
