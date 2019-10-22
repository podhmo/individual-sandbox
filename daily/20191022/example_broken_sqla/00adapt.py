from sqlalchemy.sql.sqltypes import String
from sqlalchemy.dialects.mysql.enumerated import SET

coltype = SET(length=17)
coltype.adapt(String)

print(coltype)
