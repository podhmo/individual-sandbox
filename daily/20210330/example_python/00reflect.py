import pathlib
import json
from sqlalchemy.ext.automap import automap_base
from sqlalchemy import create_engine
from sqlalchemy import event
from alchemyjsonschema import SchemaFactory
from alchemyjsonschema import StructuralWalker

dbname = pathlib.Path(__file__).parent / "data.db"
engine = create_engine("sqlite:///{}".format(dbname))


def _fk_pragma_on_connect(dbapi_con, con_record):
    dbapi_con.execute("pragma foreign_keys=ON")


event.listen(engine, "connect", _fk_pragma_on_connect)
Base = automap_base()
Base.prepare(engine, reflect=True)

schema_factory = SchemaFactory(StructuralWalker)
d = schema_factory(Base.classes.artist)
print(json.dumps(d, indent=2, ensure_ascii=False))
