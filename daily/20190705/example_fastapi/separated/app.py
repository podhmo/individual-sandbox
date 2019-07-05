import logging
from timing_asgi import TimingClient, TimingMiddleware
from timing_asgi.integrations import StarletteScopeToName

from fastapi import FastAPI

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class PrintTimings(TimingClient):
    def timing(self, metric_name, timing, tags):
        logger.info(f"{metric_name}, {timing}, {tags}")


app = FastAPI()

app.add_middleware(
    TimingMiddleware,
    client=PrintTimings(),
    metric_namer=StarletteScopeToName(prefix="app", starlette_app=app),
)
