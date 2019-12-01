import invoke

# completions: invoke --completion
# invoke --print-completion-script=bash


@invoke.task
def hello(c, name="world"):
    """hello"""
    print("hello", name)


@invoke.task
def bye(c, name="world"):
    """bye"""
    print("bye", name)
