import time
from fastapi import FastAPI
from starlette.requests import Request
from starlette.templating import Jinja2Templates
from starlette.testclient import TestClient

app = FastAPI()
templates = Jinja2Templates(directory="templates")


@app.middleware("http")
async def add_process_time_header(request: Request, call_next):
    start_time = time.time()
    response = await call_next(request)
    process_time = time.time() - start_time
    response.headers["X-Process-Time"] = str(process_time)
    return response


@app.get("/")
def read_root(request: Request):
    return templates.TemplateResponse("page.html", {"request": request})


# use starlette starlette==0.12.9, error
def test_middleware():
    client = TestClient(app)
    response = client.get("/")
    assert "X-Process-Time" in response.headers
