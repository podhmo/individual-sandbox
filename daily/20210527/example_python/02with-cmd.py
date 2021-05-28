import time
import sys
import _cmd as cmd  # for mac


class Shell(cmd.Cmd):
    def do_continue(self, arg):
        print("continue ...")
        return True

    def do_exit(self, arg):
        sys.exit(0)  # xxx


# state
i = 0

shell = Shell()
try:
    while True:
        try:
            while True:
                print(i)
                time.sleep(0.5)
                i += 1
        except KeyboardInterrupt:
            shell.cmdloop()
except KeyboardInterrupt:
    shell.onecmd("exit")
