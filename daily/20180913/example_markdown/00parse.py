import mistune
import re

body = """
#[hello] [hy]section

hello

## sub section

bar
"""


class _Renderer:
    def __init__(self, internal):
        self.internal = internal
        self.headers = []

    def __getattr__(self, name):
        return getattr(self.internal, name)

    def header(self, text, level, raw=None):
        self.headers.append((level, text))
        return self.internal.header(text, level, raw=raw)


def parse(
    body, *, renderer=None, rx=re.compile('\[[^\]]+?\]')
):
    renderer = renderer or _Renderer(mistune.Renderer())
    md = mistune.Markdown(renderer=renderer)
    md.parse(body.strip())
    level, text = next(iter(renderer.headers))
    tags = []
    title = None
    for m in rx.finditer(text.strip()):
        title = text[m.end():]
        tags.append(m.group(0))
    print("@", level, title or text, "@", tags)


parse(body)
import mistune
import re

body = """
#[hello] [hy]section

hello

## sub section

bar
"""


class _Renderer:
    def __init__(self, internal):
        self.internal = internal
        self.headers = []

    def __getattr__(self, name):
        return getattr(self.internal, name)

    def header(self, text, level, raw=None):
        self.headers.append((level, text))
        return self.internal.header(text, level, raw=raw)


def parse(
    body, *, renderer=None, rx=re.compile('\[[^\]]+?\]')
):
    renderer = renderer or _Renderer(mistune.Renderer())
    md = mistune.Markdown(renderer=renderer)
    md.parse(body.strip())
    level, text = next(iter(renderer.headers))
    tags = []
    title = None
    for m in rx.finditer(text.strip()):
        title = text[m.end():]
        tags.append(m.group(0))
    print("@", level, title or text, "@", tags)


parse(body)
