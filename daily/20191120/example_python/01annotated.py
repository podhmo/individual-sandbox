import typing_extensions as tx

RGB = tx.Annotated[tx.Literal["R", "G", "B"], object()]


def print_red(rgb: RGB) -> None:
    if rgb == "R":
        print("RED")


if __name__ == "__main__":
    print_red("R")
