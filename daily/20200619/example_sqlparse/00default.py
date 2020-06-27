import sys

pre = set(sys.modules.keys())

import moz_sql_parser

print(set(sys.modules.keys()).difference(pre))
