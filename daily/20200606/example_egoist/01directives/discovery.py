import typing as t
import logging
from egoist.app import App
from miniconfig.exceptions import Conflict
from collections import defaultdict
from egoist import runtime

NAME = __name__
logger = logging.getLogger(__name__)


class Discovery:
    """ name -> url mapping"""

    def __init__(self, *, no_conflict: bool = True) -> None:
        self.mapping: t.Dict[str, t.List[str]] = defaultdict(list)
        self.no_conflict = no_conflict

    def register(self, name: str, *, url: str, overwrite: bool = False) -> None:
        logger.info("register discovery, %s -> %s", name, url)
        if overwrite:
            self.mapping[name] = [url]
        else:
            self.mapping[name].append(url)

    def validate(self):
        if not self.no_conflict:
            return

        for name, values in self.mapping.items():
            if len(values) > 1:
                # todo: checking by app.commit()?
                assert Conflict(f"Discovery {name} is conflicted, {values}")

    def lookup(self, name: str) -> t.Optional[str]:
        values = self.mapping[name]
        if not values:
            return None
        logger.debug("lookup discovery, %s -> %s", name, values[0])
        return values[0]


def get_discovery() -> Discovery:
    return runtime.get_component(NAME)


def includeme(app: App) -> None:
    app.register_factory(NAME, Discovery)
    app.register_dryurn_factory(NAME, Discovery)
