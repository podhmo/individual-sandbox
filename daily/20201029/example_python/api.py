from __future__ import annotations
import typing as t
from starlette.applications import Starlette
from starlette.requests import Request
from starlette.responses import JSONResponse, Response
from starlette.routing import Route

if t.TYPE_CHECKING:
    from pandas.core.frame import DataFrame


def _guess_media_type(request: Request, *, default="application/json") -> str:
    format = request.query_params.get("format")
    if format == "html":
        return "text/html"
    if format == "markdown" or format == "md":
        return "text/markdown"
    elif format == "json":
        return "application/json"

    content_type = request.headers.get("content-type")
    if content_type is not None:
        return content_type
    return default


class DataframeResponse(Response):
    media_type = "application/json"

    def __init__(self, *args: t.Tuple[t.Any], **kwargs: t.Dict[str, t.Any]) -> None:
        self.to_json_orient = kwargs.pop("to_json_orient", "records")
        super().__init__(*args, **kwargs)

    def render(self, df: DataFrame) -> bytes:
        if self.media_type == "text/markdown":
            return super().render(df.to_markdown())
        elif self.media_type == "text/html":
            prefix = '<link rel="stylesheet" href="https://unpkg.com/sakura.css/css/sakura.css" type="text/css">'
            return super().render(prefix + df.to_html())
        return super().render(df.to_json(orient=self.to_json_orient))


async def list_dataset(request: Request):
    import importlib.resources

    r = []
    with importlib.resources.path("vega_datasets", "_data") as path:
        cwd = str(path.cwd())
        for fpath in path.glob("*.json"):
            name = fpath.with_suffix("").name
            r.append({name: str(fpath.absolute()).replace(cwd, ".")})
    return JSONResponse(r)


async def get_dataset(request: Request):
    from vega_datasets import data

    df: DataFrame = getattr(data, request.path_params["data"])()
    return DataframeResponse(df, media_type=_guess_media_type(request))


async def get_dataset_describe(request: Request):
    from vega_datasets import data

    df: DataFrame = getattr(data, request.path_params["data"])()
    return DataframeResponse(
        df.describe(), to_json_orient="columns", media_type=_guess_media_type(request)
    )


async def get_dataset_columns(request: Request):
    from vega_datasets import data

    df: DataFrame = getattr(data, request.path_params["data"])()
    columns = dict(zip(df.dtypes.index, df.dtypes.map(str)))
    return JSONResponse({"dataset": request.path_params["data"], "columns": columns})


async def get_dataset_aggs(request: Request):
    from vega_datasets import data

    field = request.path_params["field"]
    fns = request.query_params.getlist("fn")
    if len(fns) == 0:
        fns = ["min", "max", "mean"]

    df: DataFrame = getattr(data, request.path_params["data"])()

    grouped_df: DataFrame = (
        df.groupby(by=request.path_params["by"]).agg({field: fns}).reset_index()
    )

    # flat index
    grouped_df.columns = grouped_df.columns.to_flat_index()
    grouped_df.columns = grouped_df.columns.map(
        lambda xs: xs[0] if not xs[-1] else "-".join(xs)
    )
    return DataframeResponse(
        grouped_df,
        media_type=request.headers.get("content-type"),
    )


app = Starlette(
    debug=True,
    routes=[
        Route("/dataset", list_dataset),
        Route("/dataset/{data}", get_dataset),
        Route("/dataset/{data}/describe", get_dataset_describe),
        Route("/dataset/{data}/columns", get_dataset_columns),
        Route("/dataset/{data}/groupby/{by}/aggs/{field}", get_dataset_aggs),
    ],
)
