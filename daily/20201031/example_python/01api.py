from __future__ import annotations
import typing as t
import logging
from starlette.applications import Starlette
from starlette.requests import Request
from starlette.responses import Response, JSONResponse
from starlette.endpoints import HTTPEndpoint
from starlette.routing import Route
from starlette.background import BackgroundTask
from starlette_dataframe_response.dataset import vega_dataset_provider
from starlette_dataframe_response import DataFrameResponse

logger = logging.getLogger(__name__)


class DatasetEndpoint(HTTPEndpoint):
    async def get(self, request: Request) -> Response:
        df = vega_dataset_provider.provide_dataset("iris")
        return DataFrameResponse.from_request(request, df)

    async def _save_df(self, filename: str) -> None:
        logger.info("start")

        df = vega_dataset_provider.provide_dataset("iris")
        df2 = (
            df.groupby("species")
            .agg({"sepalLength": ["max", "min", "count", "mean"]})
            .reset_index()
        )
        df2.to_json(filename, orient="records")
        logger.info("end")

    async def post(self, request: Request) -> Response:
        logger.info("request")
        filename = (await get_json_body(request)).get("filename") or "df.json"
        task = BackgroundTask(self._save_df, filename)
        return JSONResponse({"file": filename}, background=task)


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


routes = [Route("/dataset", DatasetEndpoint)]
app = Starlette(debug=True, routes=routes, on_startup=[startup_event])
