from fastapi import FastAPI

app = FastAPI()


@app.get("/")
def hello():
    return {"message": "Hello World"}


def main():
    import os
    import uvicorn

    port = int(os.environ.get("PORT") or "8000")
    uvicorn.run(app, port=port)


if __name__ == "__main__":
    main()
