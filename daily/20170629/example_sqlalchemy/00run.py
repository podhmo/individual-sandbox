from sqlalchemy.ext.automap import automap_base
from sqlalchemy.orm import Session
from sqlalchemy import create_engine

Base = automap_base()
engine = create_engine("sqlite:///./dog.db")
Base.prepare(engine, reflect=True)
session = Session(engine)

print("table")
for table in Base.classes:
    print("  ", table.__table__.fullname)
    for row in session.query(table):
        print("    ", row.__dict__)
