import typing as t

# yapf: disable
_exc_info = t.Optional[BaseException]  # xxx
StartResponse = t.Union[
    t.Callable[[t.Text, t.List[t.Tuple[t.Text, t.Text]]], t.Callable[[bytes], None]],
    t.Callable[[t.Text, t.List[t.Tuple[t.Text, t.Text]], _exc_info], t.Callable[[bytes], None]],
]
# yapf: enable


def app(
    environ: t.Dict[str, t.Any],
    start_response: StartResponse,
) -> t.Iterable[bytes]:
    start_response('200 OK', [('Content-type', 'application/json')])
    return [b"ok"]
