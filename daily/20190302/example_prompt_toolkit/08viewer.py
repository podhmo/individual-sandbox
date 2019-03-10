from prompt_toolkit.application import Application, get_app
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.completion import PathCompleter
from prompt_toolkit.buffer import Buffer, ValidationState, ValidationError
from prompt_toolkit.lexers import PygmentsLexer
from prompt_toolkit.layout.containers import HSplit, Window, Float, FloatContainer
from prompt_toolkit.layout.controls import FormattedTextControl, BufferControl
from prompt_toolkit.layout.layout import Layout
from prompt_toolkit.layout.menus import CompletionsMenu
from prompt_toolkit.document import Document
from prompt_toolkit.widgets.toolbars import ValidationToolbar
from prompt_toolkit.widgets import TextArea, SearchToolbar
from pygments.lexers.python import PythonLexer

# todo: logger
state = {"filename": "", "content": ""}


def on_text_changed(buf: Buffer, state=state):
    global text_area
    filename = buf.text.rstrip().rsplit("\n", 1)[-1].strip()
    if filename == "":
        return
    if state["filename"] != filename:
        state["filename"] = filename
        try:
            with open(filename) as rf:
                state["content"] = rf.read()
            text_area.buffer.reset(Document(state["content"]))
            buf.validation_state = ValidationState.VALID
            buf.validation_error = None
            buf.complete_state = None
        except Exception as e:
            buf.validation_error = ValidationError(message=repr(e))
            buf.validation_state = ValidationState.INVALID


path_completer = PathCompleter(expanduser=True)
header = FormattedTextControl("Press C-c to quit")
header = FormattedTextControl("Press C-c to quit, C-x 0 switch buffer")
buffer_control = BufferControl(
    Buffer(
        on_text_changed=on_text_changed,
        complete_while_typing=True,
        completer=path_completer,
        multiline=False,
    )
)

search_field = SearchToolbar(
    text_if_not_searching=[("class:not-searching", "Press '/' to start searching.")]
)

text_area = TextArea(
    text="<no content>",
    read_only=True,
    scrollbar=True,
    line_numbers=True,
    search_field=search_field,
    lexer=PygmentsLexer(PythonLexer),
)

body = HSplit(
    [
        Window(header, height=1, style="reverse"),
        FloatContainer(
            HSplit(
                [
                    Window(content=buffer_control, height=1),
                    Window(height=1, char="-", style="class:line"),
                    text_area,
                    search_field,
                ]
            ),
            floats=[
                Float(
                    xcursor=True,
                    ycursor=True,
                    content=CompletionsMenu(max_height=12, scroll_offset=1),
                )
            ],
        ),
        ValidationToolbar(),
    ]
)


kb = KeyBindings()


@kb.add("c-c")
def do_quit(event):
    " Quit application. "
    event.app.exit()


@kb.add("c-x", "o")
def do_switch_buffer(event):
    event.app.layout.focus_next()


application = Application(layout=Layout(body), key_bindings=kb, full_screen=True)
application.run()
