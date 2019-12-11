from fastapi import FastAPI, APIRouter
from pydantic import BaseModel


router = APIRouter()


class HelloOutput(BaseModel):
    message: str


class HelloInput(BaseModel):
    name: str = "world"


@router.post("/hello", response_model=HelloOutput)
def hello(input: HelloInput):
    return {"message": f"Hello, {input.name}"}


app = FastAPI(debug=True)
app.include_router(router)
