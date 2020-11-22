import cmd


class Shell(cmd.Cmd):
    # def complete_XXX(self, text, line, begidx, endidx):
    #     pass

    def do_exit(self, line):
        print("")
        return True

    def do_EOF(self, line):
        # readline.get_completer() # => Shell.complete
        print("")
        return True

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


shell = Shell(completekey=guess_completekey())
try:
    shell.cmdloop()
except KeyboardInterrupt:
    shell.onecmd("exit")
