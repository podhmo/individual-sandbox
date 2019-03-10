from prompt_toolkit.application import Application
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.layout.containers import VSplit, HSplit, Window
from prompt_toolkit.layout.controls import FormattedTextControl, BufferControl
from prompt_toolkit.layout.layout import Layout
from prompt_toolkit.buffer import Buffer
from prompt_toolkit.document import Document


top_text = "[q] Quit [tab] switch buffer"


LIPSUM = """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Maecenas quis interdum enim. Nam viverra, mauris et blandit malesuada, ante est
bibendum mauris, ac dignissim dui tellus quis ligula. Aenean condimentum leo at
dignissim placerat. In vel dictum ex, vulputate accumsan mi. Donec ut quam
placerat massa tempor elementum. Sed tristique mauris ac suscipit euismod. Ut
tempus vehicula augue non venenatis. Mauris aliquam velit turpis, nec congue
risus aliquam sit amet. Pellentesque blandit scelerisque felis, faucibus
consequat ante. Curabitur tempor tortor a imperdiet tincidunt. Nam sed justo
sit amet odio bibendum congue. Quisque varius ligula nec ligula gravida, sed
convallis augue faucibus. Nunc ornare pharetra bibendum. Praesent blandit ex
quis sodales maximus. """


left = Window(BufferControl(Buffer(document=Document(LIPSUM))))
right = Window(BufferControl(Buffer(document=Document(LIPSUM))))


body = HSplit(
    [
        Window(FormattedTextControl(top_text), height=1, style="reverse"),
        Window(height=1, char="-"),
        VSplit([left, Window(width=1, char="|"), right]),
    ]
)


kb = KeyBindings()


@kb.add("q")
def _(event):
    " Quit application. "
    event.app.exit()


@kb.add("tab")
def _(event):
    event.app.layout.focus_next()


@kb.add("s-tab")
def _(event):
    event.app.layout.focus_previous()


application = Application(layout=Layout(body), key_bindings=kb, full_screen=True)
application.run()
