from fastapi import FastAPI

app = FastAPI()


@app.on_event("startup")
async def startup_event():
    import os
    from egoist.ext.serverprocess.spawn import FileConnectionChecker

    sentinel = os.environ.get("SENTINEL")
    if sentinel is not None:
        checker = FileConnectionChecker(sentinel=sentinel)
        assert checker.pong() is True


@app.get("/")
async def root():
    return {"message": "Hello World"}
