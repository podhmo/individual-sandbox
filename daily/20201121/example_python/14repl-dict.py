from __future__ import annotations
import typing as t
import dataclasses
import json
import sys
import cmd
import shlex
import pathlib
from handofcats import as_command


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


class Shell(cmd.Cmd):
    # def complete_XXX(self, text, line, begidx, endidx):
    #     pass
    def do_exit(self, line):
        print("")
        return True

    do_q = do_EOF = do_exit

    def cmdloop(self, intro=None):
        """Repeatedly issue a prompt, accept input, parse an initial prefix
        off the received input, and dispatch to action methods, passing them
        the remainder of the line as argument.

        """

        self.preloop()
        if self.use_rawinput and self.completekey:
            try:
                import readline

                self.old_completer = readline.get_completer()
                readline.set_completer(self.complete)
                readline.parse_and_bind(self.completekey)  # xxx
            except ImportError:
                pass
        try:
            if intro is not None:
                self.intro = intro
            if self.intro:
                self.stdout.write(str(self.intro) + "\n")
            stop = None
            while not stop:
                if self.cmdqueue:
                    line = self.cmdqueue.pop(0)
                else:
                    if self.use_rawinput:
                        try:
                            line = input(self.prompt)
                        except EOFError:
                            line = "EOF"
                    else:
                        self.stdout.write(self.prompt)
                        self.stdout.flush()
                        line = self.stdin.readline()
                        if not len(line):
                            line = "EOF"
                        else:
                            line = line.rstrip("\r\n")
                line = self.precmd(line)
                stop = self.onecmd(line)
                stop = self.postcmd(stop, line)
            self.postloop()
        finally:
            if self.use_rawinput and self.completekey:
                try:
                    import readline

                    readline.set_completer(self.old_completer)
                except ImportError:
                    pass


def guess_completekey():
    import readline

    # https://stackoverflow.com/questions/7116038/python-tab-completion-mac-osx-10-7-lion
    if "libedit" in readline.__doc__:
        return "bind ^I rl_complete"
    else:
        return "tab: complete"


class MyShell(Shell):
    def complete_send(self, text, line, begin_idx, end_idx):
        parts = shlex.split(line, posix=True)
        if text and "--name".startswith(text):
            r = ["--name"]
        elif text and "--data".startswith(text):
            r = ["--data"]
        else:
            r = [text + "@"]
        with open("/tmp/xxx", "a") as wf:
            print(
                f"$ line={parts!r} text={text!r} begin={begin_idx} end={end_idx}	=> {r}",
                file=wf,
            )
        return r

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
        except RuntimeError as e:
            return False
        print("load:", User(name=name_value, data=data_value))


shell = MyShell(completekey=guess_completekey())
try:
    shell.cmdloop()
except KeyboardInterrupt:
    shell.onecmd("exit")
