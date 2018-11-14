from prompt_toolkit import prompt
from prompt_toolkit.completion import Completer, Completion


class MyCustomCompleter(Completer):
    def get_completions(self, document, complete_event):
        # Display this completion, black on yellow.
        yield Completion('completion1', start_position=0, style='bg:ansiyellow fg:ansiblack')

        # Underline completion.
        yield Completion('completion2', start_position=0, style='underline')

        # Specify class name, which will be looked up in the style sheet.
        yield Completion('completion3', start_position=0, style='class:special-completion')


text = prompt('> ', completer=MyCustomCompleter())
