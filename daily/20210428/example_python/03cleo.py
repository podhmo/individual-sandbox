from cleo import Command
from cleo import Application


class GreetCommand(Command):
    """
    Greets someone

    greet
        {name? : Who do you want to greet?}
        {--y|yell : If set, the task will yell in uppercase letters}
    """

    def handle(self):
        run()


def f0():
    f1()


def f1():
    f2()


def f2():
    f3()


def f3():
    f4()


def f4():
    f5()


def f5():
    f6()


def f6():
    f7()


def run():
    x = locals()
    f0()


application = Application()
application.add(GreetCommand())

if __name__ == "__main__":
    application.run()
