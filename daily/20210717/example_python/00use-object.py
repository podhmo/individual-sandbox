import dataclasses
import dis
import statistics


@dataclasses.dataclass
class Point:
    x: int
    y: int


@dataclasses.dataclass
class Pair:
    left: Point
    right: Point


def use(x: int, y: int):
    left = Point(x, y)
    right = Point(x + 10, y)

    pair = Pair(left, right)
    print(statistics.median([pair.left.x, pair.left.y, pair.right.x, pair.right.y]))


dis.dis(use)
# print(dis.opmap)

for instruction in dis.Bytecode(use):
    # LOAD_GLOBAL 0 が Pointを指すってどうやってわかるんだろ？
    # 0 LOAD_GLOBAL 0 (Point)
    # 2 LOAD_FAST   0 (x)
    # 2 LOAD_FAST   1 (y)
    print("@", instruction.starts_line, instruction.opcode, instruction.opname, instruction.arg, instruction.argval, instruction.argrepr)
