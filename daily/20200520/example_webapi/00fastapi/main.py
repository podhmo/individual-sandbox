from fastapi import FastAPI
from pydantic import BaseModel


class Article(BaseModel):
    title: str
    content: str


app = FastAPI()


@app.post("/api/articles", response_model=Article)
async def create_article(article: Article):
    return article
