from __future__ import annotations
import typing as t
from prompt_toolkit import PromptSession
from prompt_toolkit.completion import Completer, Completion
from prompt_toolkit.lexers import PygmentsLexer
from pygments.lexers.shell import BashLexer

if t.TYPE_CHECKING:
    from typing import Iterable, Optional, Dict
    from prompt_toolkit.completion import CompleteEvent
    from prompt_toolkit.document import Document


class MyCompleter(Completer):
    def __init__(
        self, meta_dict: Optional[Dict[str, str]] = None, sentence: bool = False,
    ) -> None:
        self.meta_dict = meta_dict or {}
        self.sentence = sentence

    def get_completions(
        self, document: Document, complete_event: CompleteEvent
    ) -> Iterable[Completion]:
        with open("/tmp/xxx", "a") as wf:
            print(document, document.text, complete_event, file=wf)

        # Get word/text before cursor.
        if self.sentence:
            word_before_cursor = document.text_before_cursor
        else:
            word_before_cursor = document.get_word_before_cursor(
                WORD=True, pattern=None
            )

        # display_meta = self.meta_dict.get(a, "")
        display_meta = None
        for a in ["--name", "--data"]:
            yield Completion(a, -len(word_before_cursor), display_meta=display_meta)


my_completer = MyCompleter()


def main():
    session = PromptSession(lexer=PygmentsLexer(BashLexer), completer=my_completer)

    while True:
        try:
            text = session.prompt("> ")
        except KeyboardInterrupt:
            continue
        except EOFError:
            break
        else:
            print("You entered:", text)
    print("GoodBye!")


if __name__ == "__main__":
    main()
