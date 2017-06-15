from prompt_toolkit.interface import CommandLineInterface
from prompt_toolkit.application import Application
from prompt_toolkit.shortcuts import create_eventloop
from prompt_toolkit.layout.controls import BufferControl, TokenListControl
from prompt_toolkit.enums import DEFAULT_BUFFER
from pygments.token import Token
from prompt_toolkit.layout.containers import HSplit, Window

layout = HSplit(
    [
        Window(content=BufferControl(buffer_name=DEFAULT_BUFFER)),
        Window(content=TokenListControl(get_tokens=lambda cli: [(Token, 'Hello world')])),
    ]
)

loop = create_eventloop()
application = Application(layout=layout)
cli = CommandLineInterface(application=application, eventloop=loop)
cli.run()
print('Exiting')
