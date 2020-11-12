from __future__ import annotations
import typing as t
from fastapi import FastAPI
from pydantic import BaseModel


class HelloOutput(BaseModel):
    message: str


app = FastAPI()


@app.get("/", response_model=HelloOutput)
def hello() -> t.Dict[str, t.Any]:
    return HelloOutput(message="hello world")


@app.on_event("startup")
async def startup():
    import os
    import pathlib
    import time
    import sys
    import asyncio

    sentinel = os.environ.get("SENTINEL")
    if sentinel:
        st = time.time()
        p = pathlib.Path(sentinel)
        ok = False
        for wait_time in [0.1, 0.2, 0.2, 0.4, 0.8, 1.6, 3.2, 6.4]:
            if not p.exists():
                print(f"	wait ... {wait_time}", file=sys.stderr)
                await asyncio.sleep(wait_time)
                continue

            print(f"ack {sentinel}", file=sys.stderr)
            p.unlink()
            ok = True
            break
        if not ok:
            raise RuntimeError(f"timeout, f{sentinel}, {time.time()-st}")


if __name__ == "__main__":
    import uvicorn
    import os

    port = int(os.environ.get("PORT", "8888"))
    uvicorn.run(app, port=port, debug=True)
