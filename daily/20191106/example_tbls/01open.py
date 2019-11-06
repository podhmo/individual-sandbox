import sqlalchemy

uri = "sqlite:///../../20191104/example_pony/00example/estore.sqlite"
engine = sqlalchemy.create_engine(uri, echo=True)
metadata = sqlalchemy.MetaData(bind=engine)
metadata.reflect()
