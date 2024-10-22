import dis


def run(command: str) -> None:
    match command.split():
        case [action]:
            print("action0")
        case [action, obj]:
            print("action1", action, obj)
        case _:
            print("?")


run("foo0")
run("foo1 x")
with open("01.run1.output", "w") as wf:
    dis.dis(run, file=wf)
