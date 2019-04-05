from cmd import Cmd

from dictknife.mkdict import mkdict
from dictknife import loading


class Prompt(Cmd):
    prompt = "> "

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.d = {}

    def do_exit(self, inp):
        return True

    def do_mkdict(self, line):
        loading.dumpfile(mkdict(line, shared=self.d), format="json")
        print("")

    def default(self, line):
        return self.do_mkdict(line)


Prompt().cmdloop()
