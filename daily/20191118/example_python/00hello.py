import sys
import logging
import typing as t
from handofcats import as_command

logger = logging.getLogger(__name__)


@as_command
def run() -> None:
    print(t.get_type_hints(logging.basicConfig))
