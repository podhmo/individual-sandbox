from __future__ import annotations
import typing
import logging
from datetime import datetime

from prompt_toolkit import prompt
from prompt_toolkit.completion import WordCompleter
from prompt_toolkit.key_binding import KeyBindings

import logutil

if typing.TYPE_CHECKING:
    from prompt_toolkit.key_binding.key_processor import KeyPressEvent


logger = logging.getLogger(__name__)
kb = KeyBindings()
completer = WordCompleter(["foo", "bar", "boo"])


@kb.add("c-c")
@kb.add("c-d")
def do_exit(event: KeyPressEvent):
    logger.debug("event: %r", event)
    event.app.exit()


def get_toolbar():
    now = datetime.now().isoformat()
    return f"time={now}"


with logutil.run_with_logging(logger=logger):
    while True:
        text = prompt(
            "> ",
            completer=completer,
            bottom_toolbar=get_toolbar,
            refresh_interval=0.5,
            key_bindings=kb,
        )
        if text is None:
            break
        logger.info("got: %r", text)
        print(f"got {text}")
