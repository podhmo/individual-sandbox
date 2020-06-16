from fastapi import FastAPI

app = FastAPI()


d = {"i": 0}


@app.get("/")
def hello():
    d["i"] += 1
    return {"message": "hello world", "i": d["i"]}
