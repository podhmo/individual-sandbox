from prompt_toolkit import Application
from prompt_toolkit import completion
from prompt_toolkit.buffer import Buffer
from prompt_toolkit.layout import containers
from prompt_toolkit.layout import controls
from prompt_toolkit.layout import menus
from prompt_toolkit.layout.layout import Layout
from prompt_toolkit.key_binding import KeyBindings


display = None


def on_text_changed(buf):
    global display
    if buf.text != display.text:
        display.text = buf.text


animal_completer = completion.WordCompleter(
    [
        "alligator",
        "ant",
        "ape",
        "bat",
        "bear",
        "beaver",
        "bee",
        "bison",
        "butterfly",
        "cat",
        "chicken",
        "crocodile",
        "dinosaur",
        "dog",
        "dolphin",
        "dove",
        "duck",
        "eagle",
        "elephant",
        "fish",
        "goat",
        "gorilla",
        "kangaroo",
        "leopard",
        "lion",
        "mouse",
        "rabbit",
        "rat",
        "snake",
        "spider",
        "turkey",
        "turtle",
    ],
    ignore_case=True,
)


top_text = "Press C-c or C-g to quit"
buffer = Buffer(
    on_text_changed=on_text_changed,
    complete_while_typing=True,
    completer=animal_completer,
)
header = controls.FormattedTextControl(top_text)
display = controls.FormattedTextControl(text="hello world")

layout = Layout(
    containers.HSplit(
        [
            containers.Window(header, height=1, style="reverse"),
            containers.FloatContainer(
                containers.HSplit(
                    [containers.Window(content=controls.BufferControl(buffer=buffer))]
                ),
                floats=[
                    containers.Float(
                        xcursor=True,
                        ycursor=True,
                        content=menus.CompletionsMenu(max_height=16, scroll_offset=1),
                    )
                ],
            ),
            containers.Window(height=1, char="-", style="class:line"),
            containers.Window(content=display),
        ]
    )
)

kb = KeyBindings()


@kb.add("c-g")
@kb.add("c-c")
def exit_(event):
    event.app.exit()


app = Application(layout=layout, full_screen=True, key_bindings=kb)
app.run()
