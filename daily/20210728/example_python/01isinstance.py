from typing import Optional


def do(x: object) -> Optional[str]:
    if isinstance(x, (str, (int, float))):
        return "matched"
    return None


print(do(True))
x = True
print(isinstance(True, (str, (int, float))))
(x, (y, z)) = (str, (int, float))
print(x, y, z)
