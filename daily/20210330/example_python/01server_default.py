import json
from sqlalchemy import Column, Integer, String
from sqlalchemy.ext.declarative import declarative_base
from alchemyjsonschema import SchemaFactory
from alchemyjsonschema import StructuralWalker


Base = declarative_base()


class Item(Base):
    __tablename__ = "item"

    id = Column(Integer, primary_key=True)
    value0 = Column(String(255), nullable=False)
    value1 = Column(String(255), default="xxx", nullable=False)
    value2 = Column(String(255), nullable=False, server_default="'xxx'")
    value3 = Column(String(255), default="xxx", nullable=False, server_default="'xxx'")


schema_factory = SchemaFactory(StructuralWalker)
d = schema_factory(Item)
print(json.dumps(d, indent=2, ensure_ascii=False))
