from monogusa.events import subscribe, MessageEvent, Replier


@subscribe(MessageEvent)
def on_message(ev: MessageEvent, r: Replier) -> None:
    r.reply(ev, f"got {ev.content}")
