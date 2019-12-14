import databases
import sqlalchemy
from starlette.applications import Starlette
from starlette.config import Config
from starlette.responses import JSONResponse
from starlette.routing import Route

# Configuration from environment variables or '.env' file.
config = Config(".env")
DATABASE_URL = config("DATABASE_URL")


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


# Main application code.
async def list_notes(request):
    query = notes.select()
    results = await database.fetch_all(query)
    content = [
        {"text": result["text"], "completed": result["completed"]} for result in results
    ]
    return JSONResponse(content)


async def add_note(request):
    data = await request.json()
    query = notes.insert().values(text=data["text"], completed=data["completed"])
    await database.execute(query)
    return JSONResponse({"text": data["text"], "completed": data["completed"]})


routes = [
    Route("/notes", endpoint=list_notes, methods=["GET"]),
    Route("/notes", endpoint=add_note, methods=["POST"]),
]


def init_db():
    engine = sqlalchemy.create_engine(DATABASE_URL)
    metadata.create_all(bind=engine)
    engine.dispose()


app = Starlette(
    routes=routes,
    on_startup=[init_db, database.connect],
    on_shutdown=[database.disconnect],
)
