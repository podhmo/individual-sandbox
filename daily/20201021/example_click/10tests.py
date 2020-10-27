import os
import sys
import time
import click
import click_completion

click_completion.init()

option_type = click.Choice("obj1 obj2 obj3".split())


@click.group()
def cli():
    """My Cool Tool"""


@cli.group(name="object")
def object_group():
    """Object subcommand"""


@object_group.command()
@click.argument("option", type=option_type)
def get(option):
    click.echo("option: {}".format(option))


commands = (
    ('"" object get ""', 1),
    ('"" object get ""', 2),
    ('"" object get ""', 3),
    "object get obj1",
    "--help",
    "object --help",
    "object get --help",
)

os.environ["BASH_COMP"] = "complete"


time.sleep(1)
print("Click Version: {}".format(click.__version__))
print("Click Completion Version: {}".format(click_completion.__version__))
print("Python Version: {}".format(sys.version))
for cmd in commands:
    try:
        time.sleep(0.1)
        print("\n-----------")
        print("> " + str(cmd))
        time.sleep(0.1)

        if len(cmd) == 2:
            os.environ["COMP_WORDS"] = cmd[0]
            os.environ["COMP_CWORD"] = str(cmd[1])
            cli(complete_var="BASH_COMP")
        else:
            try:
                del os.environ["COMP_WORDS"]
                del os.environ["COMP_CWORD"]
            except:
                pass
            cli(cmd.split())

    except BaseException as exc:
        if str(exc) != "0" and not isinstance(exc, (click.ClickException, SystemExit)):
            raise
