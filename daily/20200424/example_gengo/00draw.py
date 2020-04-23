from graph import Builder, visualize, primitive
from handofcats import as_command


@as_command  # type: ignore
def run() -> None:
    b = Builder()

    b.add_node("Config", deps=[primitive("filename")])
    b.add_node("X", deps=["Config"])
    b.add_node("Y", deps=["Config"])
    b.add_node("Z", deps=["X", "Y"])

    g = b.build()
    print(visualize(g))
