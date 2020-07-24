import os

from fastapi import FastAPI

app = FastAPI()


@app.get("/")
def hello_world():
    target = os.environ.get("TARGET", "World")
    return {"message": f"Hello {target}!"}
