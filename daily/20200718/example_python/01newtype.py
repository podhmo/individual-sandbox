from typing import NewType
from typing_extensions import Literal

Name = NewType("Name", str)

# error: Argument 2 to NewType(...) must be subclassable
# Direction = NewType("Direction", Literal["N", "S", "W", "E"])
