import enum


@enum.unique
class RGB(enum.Enum):
    r = "R"
    g = "G"
    b = "B"

    def foo():
        print()

if __name__ == "__main__":
    print(list(RGB))
    # [<RGB.r: 'R'>, <RGB.g: 'G'>, <RGB.b: 'B'>]
    print([e.value for e in RGB.__members__.values()])
    # ['R', 'G', 'B']

    print("R" in RGB)
    # False

    print(RGB.r in RGB)
    # True

    print(RGB("R"))
    # RGB.r

    try:
        print(RGB("X"))
    except ValueError as e:
        print(e.__class__, e)
    # <class 'ValueError'> 'X' is not a valid RGB
