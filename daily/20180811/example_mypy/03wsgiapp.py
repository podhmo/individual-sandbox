import typing as t
import typing_extensions as tx


class StartResponse(tx.Protocol):
    def __call__(self, status: str, headers: t.List[t.Tuple[str, str]], exc_info: t.Any = None) -> t.Callable[[bytes], None]:
        ...


if t.TYPE_CHECKING:
    from wsgiref.types import StartResponse, WSGIEnvironment  # noqa


def app(
    environ: t.Dict[str, t.Any],
    start_response: StartResponse,
) -> t.Iterable[bytes]:
    start_response('200 OK', [('Content-type', 'application/json')])
    return [b"ok"]
