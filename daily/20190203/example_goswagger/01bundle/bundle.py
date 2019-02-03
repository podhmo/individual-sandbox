from handofcats import as_command
from dictknife import loading
from dictknife.jsonknife.resolver import get_resolver_from_filename
from dictknife.jsonknife import Bundler


@as_command
def main(*, src: str) -> None:
    resolver = get_resolver_from_filename(src)
    d = Bundler(resolver).bundle()
    loading.dumpfile(d)
