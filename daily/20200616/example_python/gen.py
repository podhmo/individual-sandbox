import importlib
import re

loader = importlib.util.find_spec("moz_sql_parser.sql_parser").loader
source = loader.get_source(loader.name)

rx = re.compile(r"\nIDENT_CHAR = .*")
print(rx.sub(lambda m: m.group(0) + "+ ':'", source, 1))
