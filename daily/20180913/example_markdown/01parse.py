import re

body = """
#[hello] [hy]section

hello

## sub section

bar
"""


def parse(body, *, rx=re.compile('\[[^\]]+?\]')):
    tags = []
    title = ""
    itr = iter(body.split("\n"))
    for line in itr:
        if line.lstrip().startswith("#") and not line.lstrip().startswith("##"):
            for m in rx.finditer(line.strip()):
                title = line[m.end():]
                tags.append(m.group(0).strip("[ ]"))
            break
    content = "\n".join(itr)
    print("@", title, "@", tags, "@", content.lstrip)


parse(body)
