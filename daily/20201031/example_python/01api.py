from starlette.applications import Starlette
from starlette.requests import Request
from starlette.responses import Response, JSONResponse
from starlette.endpoints import HTTPEndpoint
from starlette.routing import Route
from starlette.background import BackgroundTask
from starlette_dataframe_response.dataset import vega_dataset_provider
from starlette_dataframe_response import DataFrameResponse


class DatasetEndpoint(HTTPEndpoint):
    def get(self, request: Request) -> Response:
        df = vega_dataset_provider.provide_dataset("iris")
        return DataFrameResponse.from_request(request, df)

    def post(self, request: Request) -> Response:
        print("----------------------------------------")
        print("request")
        print("----------------------------------------")

        async def save_df(filename: str):
            print("----------------------------------------")
            print("start")
            print("----------------------------------------")

            df = vega_dataset_provider.provide_dataset("iris")
            df2 = (
                df.groupby("species")
                .agg({"sepalLength": ["max", "min", "count", "mean"]})
                .reset_index()
            )
            df2.to_json(filename, orient="records")
            print("----------------------------------------")
            print("end  ")
            print("----------------------------------------")

        task = BackgroundTask(save_df, filename="df.json")
        return JSONResponse({"file": "df.json"}, background=task)


app = Starlette(debug=True, routes=[Route("/dataset", DatasetEndpoint)])
