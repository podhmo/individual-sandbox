from handofcats import as_subcommand as register


@register
def hello(*, name: str = "world"):
    print(f"hello {name}")


@register
def byebye(name):
    print(f"byebye {name}")


register.run()
