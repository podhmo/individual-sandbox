import typing as t
from monogusa import events
from monogusa import reactions


@events.subscribe(events.MessageEvent[t.Any])
def on_message(ev: events.MessageEvent[t.Any], reply: reactions.reply_message) -> None:
    reply(ev, f"got: {ev.content}")


@events.subscribe(events.MessageEvent[t.Any])
def on_message2(ev: events.MessageEvent[t.Any], send: reactions.send_message) -> None:
    send(ev, f"send: {ev.content}", channel="random")
