from __future__ import annotations
import typing as t
import shlex
import pathlib
import dataclasses
import cmd
import json
import sys
from prompt_toolkit import PromptSession
from prompt_toolkit.completion import Completer, Completion
from prompt_toolkit.lexers import PygmentsLexer
from pygments.lexers.shell import BashLexer

if t.TYPE_CHECKING:
    from typing import Iterable, Optional, Dict
    from prompt_toolkit.completion import CompleteEvent
    from prompt_toolkit.document import Document


@dataclasses.dataclass
class User:
    name: str
    data: t.Dict[str, t.Any]


def load_dict(
    filename_or_string: str,
    *,
    fileprefix: str = "@",
    loads: t.Callable[[str], t.Dict[str, t.Any]] = json.loads,
) -> t.Dict[str, t.Any]:
    if filename_or_string.startswith(fileprefix):
        filename = filename_or_string[1:]
        with open(filename) as rf:
            s = rf.read()
    else:
        s = filename_or_string
    return loads(s)


class MyShell(cmd.Cmd):
    def do_exit(self, line):
        print("")
        return True

    do_q = do_EOF = do_exit

    def do_send(self, line):
        """
        examples:
        send --name foo --data=@data.json
        """
        import argparse

        class ArgumentParser(argparse.ArgumentParser):
            def exit(self, status=0, message=None):
                if message:
                    self._print_message(message, sys.stderr)
                raise RuntimeError(str(message or status))

        parser = ArgumentParser(
            formatter_class=type(
                "_HelpFormatter",
                (argparse.ArgumentDefaultsHelpFormatter, argparse.RawTextHelpFormatter),
                {},
            )
        )
        parser.print_usage = parser.print_help  # type: ignore
        parser.add_argument("--name", required=True)
        parser.add_argument("--data", required=True, help="json string or @filename")

        parts = shlex.split(line)
        try:
            args = parser.parse_args(parts)

            name_value = args.name
            data_value = load_dict(
                args.data
            )  # todo: validation error (FileNotFoundError)
        except FileNotFoundError as e:
            print("!!", e)
            return False
        except RuntimeError:
            return False
        print("load:", User(name=name_value, data=data_value))


class MyCompleter(Completer):
    def __init__(self, meta_dict: Optional[Dict[str, str]] = None,) -> None:
        self.meta_dict = meta_dict or {}

    def get_completions(
        self, document: Document, complete_event: CompleteEvent
    ) -> Iterable[Completion]:
        parts = shlex.split(document.text.lstrip(" "))
        suffix = document.text[len(document.text.rstrip(" ")) :]
        if suffix:
            parts.append(suffix)

        with open("/tmp/xxx", "a") as wf:
            print(document, "@", parts, "@", complete_event, file=wf)

        # Get word/text before cursor.
        word_before_cursor = document.get_word_before_cursor(WORD=True, pattern=None)

        # display_meta = self.meta_dict.get(a, "")
        display_meta = None

        if not parts:
            for a in ["send", "recv"]:
                yield Completion(a, -len(word_before_cursor), display_meta=display_meta)
        elif len(parts) == 1 and not parts[0].startswith("-"):
            for a in ["send", "recv"]:
                if a.startswith(parts[0]):
                    yield Completion(
                        a, -len(word_before_cursor), display_meta=display_meta
                    )
        elif parts[-1].startswith("@"):
            suffix = parts[-1][1:]
            if "/" in suffix:
                dirpath = pathlib.Path(suffix).parent
            else:
                dirpath = pathlib.Path(".")
            for a in dirpath.iterdir():
                if not suffix or suffix in str(a):
                    yield Completion(
                        f"@{a}", -len(word_before_cursor), display_meta=display_meta
                    )
        elif parts[-1].startswith("-"):
            for a in ["--name", "--data"]:
                if a.startswith(parts[-1]):
                    yield Completion(
                        a, -len(word_before_cursor), display_meta=display_meta
                    )
        elif not parts[-1].strip() and parts[-2] == "--data":
            yield Completion(
                "@", -len(word_before_cursor), display_meta=display_meta,
            )
        else:
            return []


my_completer = MyCompleter()


def main():
    session = PromptSession(lexer=PygmentsLexer(BashLexer), completer=my_completer)
    myshell = MyShell()

    while True:
        try:
            text = session.prompt("> ")
        except KeyboardInterrupt:
            continue
        except EOFError:
            break
        else:
            print("You entered:", text)
            myshell.onecmd(text)
    print("GoodBye!")


if __name__ == "__main__":
    main()
