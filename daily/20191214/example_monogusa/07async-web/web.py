from magicalimport import import_module
from fastapi import FastAPI

routers = import_module("./routers.py", here=__file__)
app = FastAPI()
app.include_router(routers.router)


if __name__ == "__main__":
    import monogusa.web.cli as webcli

    webcli.run(app)
