from pydantic import BaseModel
from pydantic.schema import model_process_schema
from dictknife import loading


class User(BaseModel):
    id: str = None
    name: str
    createdAt: str = None  # datetime


REF_PREFIX = "#/components/schemas/"
model_name_map = {}
m_schema, m_definitions = model_process_schema(
    User, model_name_map=model_name_map, ref_prefix=REF_PREFIX
)
print("## m_schema")
loading.dumpfile(m_schema)
print("## m_definitions")
loading.dumpfile(m_definitions)
