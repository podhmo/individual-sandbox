from fastapi import FastAPI
from fastapi import Request

app = FastAPI()


@app.get("/{rest}")
def all(req: Request):
    return {"path": req.url}
