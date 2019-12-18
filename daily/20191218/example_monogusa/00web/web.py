from routes import router
from fastapi import FastAPI

app = FastAPI()
app.include_router(router)

if __name__ == "__main__":
    from monogusa.web.cli import run

    run(app)
