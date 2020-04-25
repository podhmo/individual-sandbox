from graph import Builder, primitive
from graph import visualize
from handofcats import as_command


@as_command  # type: ignore
def run() -> None:
    b = Builder()

    b.add_node("Config", depends=[primitive("filename")])
    b.add_node("X", depends=["Config"])
    b.add_node("Y", depends=["Config"])
    b.add_node("Z", depends=["X", "Y"])

    g = b.build()
    print(visualize(g))
