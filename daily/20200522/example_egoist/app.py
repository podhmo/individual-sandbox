import typing as t
import typing_extensions as tx
from functools import partial
from marker import Marker


class Metadata(tx.TypedDict):
    path: str
    method: str
    tags: t.List[str] = None
    summary: str = None
    description: str = None

    # response_model: Type[Any] = None
    # status_code: int = 200
    # dependencies: Sequence[Depends] = None
    # response_description: str = "Successful Response"
    # responses: Dict[Union[int, str], Dict[str, Any]] = None
    # deprecated: bool = None
    # operation_id: str = None
    # response_model_include: Union[SetIntStr, DictIntStrAny] = None
    # response_model_exclude: Union[SetIntStr, DictIntStrAny] = set()
    # response_model_by_alias: bool = True
    # response_model_skip_defaults: bool = None
    # response_model_exclude_unset: bool = False
    # response_model_exclude_defaults: bool = False
    # response_model_exclude_none: bool = False
    # include_in_schema: bool = True
    # response_class: Type[Response] = None
    # name: str = None
    # callbacks: List[routing.APIRoute] = None


class App:
    def __init__(self) -> None:
        self._zero_metadata: Metadata = {}
        self._marker = Marker("api_route", default=self._zero_metadata)

    def _register_with(
        self, fn: t.Callable[..., t.Any], *, method: str, path: str, metadata: Metadata
    ) -> t.Callable[..., t.Any]:
        metadata["method"] = method.lower()
        metadata["path"] = path
        k = f'{metadata["method"]}::{metadata["path"]}'
        return self._marker.mark(fn, name=k, val=metadata)

    def post(
        self, path: str, metadata: t.Optional[Metadata] = None,
    ) -> t.Callable[[t.Callable[..., t.Any]], t.Callable[..., t.Any]]:
        metadata: Metadata = metadata or {}
        return partial(self._register_with, method="post", path=path, metadata=metadata)

    @property
    def routes(self) -> t.Iterator[t.Tuple[t.Callable[..., t.Any], Metadata]]:
        for fns in self._marker.pool.values():
            for fn in fns:
                metadata = self._marker(fn)
                yield fn, metadata
