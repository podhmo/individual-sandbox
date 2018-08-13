from typing import List, Tuple, Any, Callable, TYPE_CHECKING, Dict, Iterable, Optional
from typing_extensions import Protocol


class StartResponse(Protocol):
    def __call__(
        self,
        status: str,
        headers: List[Tuple[str, str]],
        # **extra: Any,
        exc_info: Optional[Any] = None,
    ) -> Callable[[bytes], None]:
        ...


if TYPE_CHECKING:
    from wsgiref.types import StartResponse, WSGIEnvironment  # noqa


def app(
    environ: Dict[str, Any],
    start_response: StartResponse,
) -> Iterable[bytes]:
    start_response('200 OK', [('Content-type', 'application/json')])
    return [b"ok"]
