from __future__ import annotations
import typing
from prompt_toolkit.key_binding import KeyBindings

if typing.TYPE_CHECKING:
    from prompt_toolkit.key_binding.key_processor import KeyPressEvent

kb = KeyBindings()


@kb.add("q")
def do_quit(event: KeyPressEvent):  # type hintを追加
    " Quit application. "
    # event.| ここで補完が効くようになる
    event.app.exit()
