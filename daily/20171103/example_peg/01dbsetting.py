# sample: https://github.com/kennethreitz/dj-database-url
"""
PostgreSQL
	postgres://USER:PASSWORD@HOST:PORT/NAME
MSSQL
	mssql://USER:PASSWORD@HOST:PORT/NAME
MySQL
	mysql://USER:PASSWORD@HOST:PORT/NAME
SQLite
	sqlite:///PATH
SpatiaLite
	spatialite:///PATH
Oracle
	oracle://USER:PASSWORD@HOST:PORT/NAME
Redshift
	redshift://USER:PASSWORD@HOST:PORT/NAME
"""

from parsimonious.grammar import Grammar
grammar = Grammar(
    r"""
dburl = scheme '://' (authinfo '@')? hostinfo? '/' path
scheme = token
authinfo = username ':' password
username = token
password = token
hostinfo = (host ':')? port
host = token
port = token
path = token
token = ~"[A-Z 0-9]+"i
"""
)

parsed = grammar.parse("postgres://USER:PASSWORD@HOST:PORT/NAME")
print(parsed)

parsed = grammar.parse("postgres://HOST:PORT/NAME")
print(parsed)

parsed = grammar.parse("postgres://HOST/NAME")
print(parsed)

parsed = grammar.parse("postgres:///NAME")
print(parsed)

