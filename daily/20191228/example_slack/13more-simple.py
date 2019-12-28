import sys
import os
from monogusa import ignore
from monogusa.chatbot import slackchat


def hello(*, name: str = "world") -> None:
    """hello message"""
    print(f"hello {name}")


def byebye(*, name: str = "world") -> None:
    """byebye :wave:"""
    print(f"byebye {name}")


def ng(*, name: str = "world") -> None:
    """:warning: NG"""
    1 / 0


@ignore
def main():
    import dotenv
    import logging

    logging.basicConfig(level=logging.DEBUG)
    module = sys.modules[__name__]

    dotenv.load_dotenv(verbose=True)
    token = os.environ["SLACKBOT_API_TOKEN"]
    slackchat.run(token, module=module, name="app")


if __name__ == "__main__":
    main()
