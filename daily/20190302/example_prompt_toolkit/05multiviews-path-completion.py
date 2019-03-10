from prompt_toolkit import Application
from prompt_toolkit import completion
from prompt_toolkit.buffer import Buffer, ValidationState, ValidationError
from prompt_toolkit.layout import containers
from prompt_toolkit.layout import controls
from prompt_toolkit.layout import menus
from prompt_toolkit.layout.layout import Layout
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.widgets.toolbars import ValidationToolbar

state = {"filename": "", "content": ""}
content_display = None
current_display = None


def on_text_changed(buf: Buffer, state=state):
    filename = buf.text.rstrip().rsplit("\n", 1)[-1].strip()
    if filename == "":
        return
    if state["filename"] != filename:
        # state["filename"] = current_display.text = filename
        try:
            with open(filename) as rf:
                state["content"] = content_display.text = rf.read()
            buf.validation_state = ValidationState.VALID
            buf.validation_error = None
        except FileNotFoundError as e:
            buf.validation_error = ValidationError(message=repr(e))
            buf.validation_state = ValidationState.INVALID


class _Wrap:
    def __init__(self, doc):
        global current_display
        self.doc = doc
        current_display.text = f"{doc.text_before_cursor}@{doc.text_after_cursor}"
        self.text_before_cursor = doc.text_before_cursor

    def __getattr__(self, name):
        return getattr(self.doc, name)


class PathCompleter(completion.PathCompleter):
    def get_completions(self, document, complete_event):
        return super().get_completions(_Wrap(document), complete_event)


path_completer = PathCompleter(expanduser=True)

top_text = "Press C-d or C-c or C-g to quit"
buffer = Buffer(
    on_text_changed=on_text_changed,
    complete_while_typing=True,
    completer=path_completer,
)
header = controls.FormattedTextControl(top_text)
content_display = controls.FormattedTextControl(text="hello world")
current_display = controls.FormattedTextControl(text="")

layout = Layout(
    containers.HSplit(
        [
            containers.Window(header, height=1, style="reverse"),
            containers.FloatContainer(
                containers.HSplit(
                    [
                        containers.Window(
                            content=controls.BufferControl(buffer=buffer), height=1
                        ),
                        containers.Window(height=1, char="-", style="class:line"),
                        containers.Window(content=current_display, height=1),
                        containers.Window(height=1, char="-", style="class:line"),
                        containers.Window(content=content_display),
                    ]
                ),
                floats=[
                    containers.Float(
                        xcursor=True,
                        ycursor=True,
                        content=menus.CompletionsMenu(max_height=12, scroll_offset=1),
                    )
                ],
            ),
            ValidationToolbar(),
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
