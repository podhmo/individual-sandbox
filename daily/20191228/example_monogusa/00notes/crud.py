import typing as t
from databases import Database
from models import notes


async def create_note(db: Database, *, text: str, completed: bool = False) -> t.Mapping:
    query = notes.insert().values(text=text, completed=completed)
    last_record_id = await db.execute(query)
    return {"text": text, "completed": completed, "id": last_record_id}


async def read_notes(db: Database) -> t.List[t.Mapping]:
    query = notes.select()
    return await db.fetch_all(query)
