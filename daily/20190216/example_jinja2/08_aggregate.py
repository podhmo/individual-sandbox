import jinja2
import linecache
from kamidana.debug._extract import extract_information


t = jinja2.Template(
    """
::::::::::
{{1/0}}
::::::::::
"""
)

try:
    t.filename = "main.j2"
    t.render()
except Exception as e:
    info = extract_information(e)
    print(info)
    print(repr(linecache.getline(info.filename, lineno=1)))
