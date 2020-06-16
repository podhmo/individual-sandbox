from fastapi import FastAPI

app = FastAPI()


d = {"i": 0}


@app.get("/")
def hello():
    d["i"] += 1
    return {"message": "hello world", "i": d["i"]}


@app.on_event("startup")
async def startup_event():
    import os
    from egoist.ext.serverprocess.spawn import FileConnectionChecker

    sentinel = os.environ.get("SENTINEL")
    if sentinel is not None:
        checker = FileConnectionChecker(sentinel=sentinel)
        assert checker.pong() is True
