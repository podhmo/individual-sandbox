import typing as t
import databases
import sqlalchemy
from fastapi import FastAPI
from pydantic import BaseModel
from starlette.config import Config

# Configuration from environment variables or '.env' file.
config = Config(".env")
DATABASE_URL = config("DATABASE_URL")
# DATABASE_URL = "postgresql://user:password@postgresserver/db"

# Database table definitions.
metadata = sqlalchemy.MetaData()
notes = sqlalchemy.Table(
    "notes",
    metadata,
    sqlalchemy.Column("id", sqlalchemy.Integer, primary_key=True),
    sqlalchemy.Column("text", sqlalchemy.String),
    sqlalchemy.Column("completed", sqlalchemy.Boolean),
)
database = databases.Database(DATABASE_URL)


class NoteIn(BaseModel):
    text: str
    completed: bool


class Note(BaseModel):
    id: int
    text: str
    completed: bool


app = FastAPI()


@app.on_event("startup")
async def startup():
    engine = sqlalchemy.create_engine(
        DATABASE_URL, connect_args={"check_same_thread": False}
    )
    metadata.create_all(bind=engine)
    engine.dispose()

    await database.connect()


@app.on_event("shutdown")
async def shutdown():
    await database.disconnect()


@app.get("/notes/", response_model=t.List[Note])
async def read_notes():
    query = notes.select()
    return await database.fetch_all(query)


@app.post("/notes/", response_model=Note)
async def create_note(note: NoteIn):
    query = notes.insert().values(text=note.text, completed=note.completed)
    last_record_id = await database.execute(query)
    return {**note.dict(), "id": last_record_id}
