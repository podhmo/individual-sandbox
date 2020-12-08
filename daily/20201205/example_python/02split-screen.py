from __future__ import annotations
import typing as t
import asyncio
from io import StringIO
from prompt_toolkit.application import Application
from prompt_toolkit.buffer import Buffer
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.layout.containers import HSplit, VSplit, Window, WindowAlign
from prompt_toolkit.layout.controls import BufferControl, FormattedTextControl
from prompt_toolkit.layout.layout import Layout

if t.TYPE_CHECKING:
    from prompt_toolkit.key_binding.key_processor import KeyPressEvent


def get_titlebar_text():
    return [
        ("class:title", " Hello world "),
        ("class:title", " (Press [Ctrl-Q] to quit.)"),
    ]


left_buffer = Buffer()
right_buffer = Buffer()

left_window = Window(BufferControl(buffer=left_buffer), height=2)
right_window = Window(BufferControl(buffer=right_buffer))


body = VSplit(
    [
        HSplit(
            [
                left_window,
                Window(
                    height=1,
                    content=FormattedTextControl(lambda: [("class:title", "(o_0)")]),
                ),
            ]
        ),
        Window(width=1, char="|", style="class:line"),
        right_window,
    ]
)

root_container = HSplit(
    [
        Window(
            height=1,
            content=FormattedTextControl(get_titlebar_text),
            align=WindowAlign.CENTER,
        ),
        Window(height=1, char="-", style="class:line"),
        body,
    ]
)

kb = KeyBindings()


@kb.add("c-c", eager=True)
@kb.add("c-q", eager=True)
def _(event: KeyPressEvent) -> None:
    event.app.exit()


application = Application(
    layout=Layout(root_container, focused_element=left_window),
    key_bindings=kb,
    mouse_support=True,
    full_screen=True,
)

o = StringIO()


async def tick():
    for i in range(100):
        print("tick", i, file=o)
        await asyncio.sleep(0.5)


async def redraw():
    while True:
        right_buffer.text = o.getvalue()
        right_buffer.auto_down()
        await asyncio.sleep(0.6)


application.create_background_task(tick())
application.create_background_task(redraw())
application.run()
