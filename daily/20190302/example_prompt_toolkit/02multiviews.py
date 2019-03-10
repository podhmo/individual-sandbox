from prompt_toolkit import Application
from prompt_toolkit.buffer import Buffer
from prompt_toolkit.layout import containers
from prompt_toolkit.layout import controls
from prompt_toolkit.layout.layout import Layout
from prompt_toolkit.key_binding import KeyBindings


display = None


def on_text_changed(buf):
    global display
    if buf.text != display.text:
        display.text = buf.text


top_text = "Press C-c to quit"
buffer = Buffer(on_text_changed=on_text_changed)
header = controls.FormattedTextControl(top_text)
display = controls.FormattedTextControl(text="hello world")

layout = Layout(
    containers.HSplit(
        [
            containers.Window(header, height=1, style="reverse"),
            containers.Window(content=controls.BufferControl(buffer=buffer)),
            containers.Window(content=display),
        ]
    )
)

kb = KeyBindings()


@kb.add("c-c")
def exit_(event):
    event.app.exit()


app = Application(layout=layout, full_screen=True, key_bindings=kb)
app.run()
