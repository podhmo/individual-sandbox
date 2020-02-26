import typing as t
from monogusa import events
from monogusa import reactions


@events.subscribe(events.MessageEvent[t.Any])
def on_message(ev: events.MessageEvent[t.Any], reply: reactions.reply_message) -> None:
    reply(ev, f"got: {ev.content}")


@events.subscribe(events.MessageEvent[t.Any])
def on_message2(ev: events.MessageEvent[t.Any], send: reactions.send_message) -> None:
    send(ev, f"send: {ev.content}", channel="random")


def sample() -> None:
    """fake event stream"""
    from monogusa.cli.events import console_stream, run_stream

    examples = """\
Message, hello
Message, byebye
"""
    run_stream(console_stream(default=examples, sep=","))


if __name__ == "__main__":
    from monogusa.cli import run

    run()
