from __future__ import annotations
import typing as t
import logging
import pathlib
import time
from starlette.applications import Starlette
from starlette.requests import Request
from starlette.responses import Response, JSONResponse
from starlette.routing import Route
from starlette.background import BackgroundTask
from starlette_dataframe_response.dataset import vega_dataset_provider
from starlette_dataframe_response import DataFrameResponse

if t.TYPE_CHECKING:
    from pandas.core.frame import DataFrame
logger = logging.getLogger(__name__)


def get_dataset(request: Request) -> Response:
    dataset_name = request.path_params["dataset_name"]
    df = vega_dataset_provider.provide_dataset(dataset_name)
    return DataFrameResponse.from_request(request, df)


async def create_dataset_aggs(request: Request) -> Response:
    logger.info("request")
    dataset_name = request.path_params["dataset_name"]
    by = request.path_params["by"]
    fields = [request.path_params["field"]]
    fns = request.query_params.getlist("fn")
    if not fns:
        fns = ["min", "max", "mean"]

    filename = (await get_json_body(request)).get("filename") or "df.json"
    task = BackgroundTask(
        _save_df,
        filename=filename,
        fields=fields,
        by=by,
        fns=fns,
        dataset_name=dataset_name,
        st=time.time(),
    )
    return JSONResponse({"file": filename}, background=task)


async def _save_df(
    *,
    filename: str,
    dataset_name: str,
    fields: t.List[str],
    fns: t.List[str],
    by: str,
    st: time.time,
) -> None:
    logger.info("start")

    df: DataFrame = vega_dataset_provider.provide_dataset(dataset_name)

    if "," in fields[0]:
        fields = [f.strip() for f in fields[0].split(",")]

    grouped_df: DataFrame = (
        df.groupby(by=by).agg({f: fns for f in fields}).reset_index()
    )

    # flat index
    grouped_df.columns = grouped_df.columns.to_flat_index()
    grouped_df.columns = grouped_df.columns.map(
        lambda xs: xs[0] if not xs[-1] else "-".join(xs)
    )
    pathlib.Path(filename).parent.mkdir(exist_ok=True, parents=True)
    grouped_df.to_json(filename)
    logger.info("end -- %s", time.time() - st)


async def get_json_body(request: Request) -> t.Dict:
    import json

    try:
        return await request.json()
    except json.JSONDecodeError:
        return {}


async def startup_event():
    handler = logging.StreamHandler()
    handler.setFormatter(logging.Formatter("%(asctime)s - %(levelname)s - %(message)s"))

    # TODO: disable uvicorn default logger

    root_logger = logging.getLogger()
    root_logger.addHandler(handler)
    root_logger.setLevel(logging.DEBUG)


async def pong():
    import os
    import sys

    sentinel_file = os.environ.get("SENTINEL")
    if sentinel_file is None:
        print(f"	** sentinel is not found. skip.", file=sys.stderr)
        return
    print(f"	** sentinel is found. removing (ack). {sentinel_file}", file=sys.stderr)
    os.path.exists(sentinel_file) and os.remove(sentinel_file)


routes = [
    Route("/dataset/{dataset_name}", get_dataset),
    Route(
        "/dataset/{dataset_name}/groupby/{by}/aggs/{field}",
        create_dataset_aggs,
        methods=["POST"],
    ),
]
app = Starlette(debug=True, routes=routes, on_startup=[startup_event, pong])
