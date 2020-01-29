class Section:
    def __init__(self, title: str, body: str):
        self.title = title
        self.body = body

    def to_text(self):
        raise NotImplementedError("")


class MarkdownSection(Section):
    def to_text(self):
        return f"""\
# {self.title}

{self.body}
"""


class HTMLSection(Section):
    def to_text(self):
        return f"""\
<h1>{self.title}</h1>
<p>{self.body}</p>
"""


section = MarkdownSection("foo", "hello, this is my first document")
print(section.to_text())
section = HTMLSection("foo", "hello, this is my first document")
print(section.to_text())
