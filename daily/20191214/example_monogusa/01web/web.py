import typing as t
import contextlib
import time
from io import StringIO
from magicalimport import import_module
from fastapi import APIRouter, FastAPI
from pydantic import BaseModel

cli = import_module("./cli.py", here=__file__)
router = APIRouter()


def _update_docs(fn):
    def attach(target_fn):
        if target_fn is None:
            target_fn.__doc__ = fn.__doc__
        return target_fn

    return attach


def _as_list(s: str) -> t.List[str]:
    s = s.rstrip()
    return s.split("\n") if s else []


class CommandOutput(BaseModel):
    stdout: t.List[str]
    stderr: t.List[str]
    duration: float


class HelloInput(BaseModel):
    name: str


@router.post("/hello", response_model=CommandOutput)
@_update_docs(cli.hello)
def hello(input: HelloInput) -> t.Dict[str, t.Any]:
    st = time.time()
    stdout = StringIO()
    stderr = StringIO()
    # TODO: use demux like interface, instead of redirect_xxx()
    with contextlib.redirect_stdout(stdout):
        with contextlib.redirect_stderr(stderr):
            cli.hello(**input.dict())  # TODO: support positional arguments?
    return {
        "duration": time.time() - st,
        "stdout": _as_list(stdout.getvalue()),
        "stderr": _as_list(stderr.getvalue()),
    }


class ByeInput(BaseModel):
    name: str


@router.post("/bye", response_model=CommandOutput)
@_update_docs(cli.bye)
def bye(input: ByeInput) -> t.Dict[str, t.Any]:
    st = time.time()
    stdout = StringIO()
    stderr = StringIO()
    # TODO: use demux like interface, instead of redirect_xxx()
    with contextlib.redirect_stdout(stdout):
        with contextlib.redirect_stderr(stderr):
            cli.bye(**input.dict())  # TODO: support positional arguments?
    return {
        "duration": time.time() - st,
        "stdout": _as_list(stdout.getvalue()),
        "stderr": _as_list(stderr.getvalue()),
    }


app = FastAPI()
app.include_router(router)


def main():
    import argparse
    import os

    parser = argparse.ArgumentParser()
    parser.add_argument("--show-doc", action="store_true")
    parser.add_argument("--debug", action="store_true")
    parser.add_argument("--port", type=int, default=None)
    args = parser.parse_args()

    if args.show_doc:
        return show_doc(debug=args.debug)
    else:
        os.chdir(os.path.dirname(__file__))
        app_name = os.path.basename(__file__)[: -len(".py")]
        cmd_args = ["uvicorn", f"{app_name}:app"]
        if args.debug:
            cmd_args.append("--debug")
        if args.port is not None:
            cmd_args.extend(["--port", str(args.port)])
        return os.execvp("uvicorn", cmd_args)


def show_doc(*, debug: bool = False):
    import asyncio
    from async_asgi_testclient import TestClient
    from dictknife import loading

    async def run():
        async with TestClient(app) as client:
            response = await client.get("/openapi.json")
            loading.dumpfile(response.json())

    asyncio.run(run(), debug=debug)


if __name__ == "__main__":
    main()
