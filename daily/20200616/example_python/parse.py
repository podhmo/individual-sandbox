import moz_sql_parser
from sqlparser import SQLParser

moz_sql_parser.SQLParser = SQLParser
parse = moz_sql_parser.parse
