from prompt_toolkit.application import Application
from prompt_toolkit.buffer import Buffer
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.layout.containers import HSplit, VSplit, Window, WindowAlign
from prompt_toolkit.layout.controls import BufferControl, FormattedTextControl
from prompt_toolkit.layout.layout import Layout

left_buffer = Buffer()
right_buffer = Buffer()

left_window = Window(BufferControl(buffer=left_buffer))
right_window = Window(BufferControl(buffer=right_buffer))


body = VSplit(
    [left_window, Window(width=1, char="|", style="class:line"), right_window,]
)


def get_titlebar_text():
    return [
        ("class:title", " Hello world "),
        ("class:title", " (Press [Ctrl-Q] to quit.)"),
    ]


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
def _(event):
    """
    Pressing Ctrl-Q or Ctrl-C will exit the user interface.

    Setting a return value means: quit the event loop that drives the user
    interface and return this value from the `Application.run()` call.

    Note that Ctrl-Q does not work on all terminals. Sometimes it requires
    executing `stty -ixon`.
    """
    event.app.exit()


def default_buffer_changed(_):
    right_buffer.text = left_buffer.text[::-1]


left_buffer.on_text_changed += default_buffer_changed


application = Application(
    layout=Layout(root_container, focused_element=left_window),
    key_bindings=kb,
    mouse_support=True,
    full_screen=True,
)


def run():
    application.run()


if __name__ == "__main__":
    run()
