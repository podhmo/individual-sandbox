import foo
import foo as bar
from foo import boo
import foo.boo
import foo.boo as boo
from foo import (
    x,
)
from foo import (
    x,
)
from foo import (
    x as X,
    y,
)
from foo import (
    x as X,
    y,
)
from foo import (
    x as X,
    y as Y,
)
from foo import (
    x as X,
    y as Y,
)
import foo  # NOQA
import foo as bar  # NOQA
from foo import boo  # NOQA
import foo.boo  # NOQA
import foo.boo as boo  # NOQA
from foo import (  # NOQA
    x,
)
from foo import (  # NOQA
    x,
)
from foo import (  # NOQA
    x as X,
    y,
)
from foo import (  # NOQA
    x as Y,
    y,
)
from foo import (  # NOQA
    x as X,
    y as Y,
)
from foo import (  # NOQA
    x as X,
    y as Y,
)
