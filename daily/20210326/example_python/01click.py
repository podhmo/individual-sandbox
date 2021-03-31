import click


@click.group(invoke_without_command=True)
@click.pass_context
def cli(ctx):
    if ctx.invoked_subcommand is None:
        print(ctx.get_help())
    else:
        print("gonna invoke %s" % ctx.invoked_subcommand)


@cli.command(help="description 1")
@click.argument("target", required=False)
def hello(target):
    print("hello", target)


@cli.command(help="description 2")
@click.argument("target", required=False)
def byebye(target):
    print("byebye", target)


if __name__ == "__main__":
    cli()
