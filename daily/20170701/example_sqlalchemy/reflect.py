from sqlalchemy import create_engine, MetaData

engine = create_engine("sqlite:///xxx.db")
metadata = MetaData()
print(vars(metadata))
metadata.reflect(engine)
print(vars(metadata))
