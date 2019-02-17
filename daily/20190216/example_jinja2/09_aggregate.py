import jinja2
import linecache
from kamidana.debug._extract import extract_information

try:
    env = jinja2.Environment(loader=jinja2.FileSystemLoader("."))
    t = env.get_template("02include-ng/zero.div.j2")
    print(t.render())
except Exception as e:
    info = extract_information(e)
    print(info)
    print(info.outermost)
    for i in range(max(1, info.lineno - 3), info.lineno + 3 + 1):
        line = linecache.getline(info.filename, lineno=i)
        if not line:
            continue
        print(i, line, end="")
