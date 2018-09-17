import re
import os.path
import mistune


class Captured:
    def __init__(self, internal):
        self.internal = internal
        self.replacers = []

    def __getattr__(self, name):
        # print("@", name)
        return getattr(self.internal, name)

    def image(self, src, title, text):
        self.replacers.append(
            (re.compile(f"\( *{src} *\)"), f"(http://example.com/images/{os.path.basename(src)})")
        )
        print("!", src, title, text)
        return self.internal.image(src, title, text)


caputured = Captured(mistune.Renderer())
m = mistune.Markdown(renderer=caputured)

with open("src/doc.md") as rf:
    text = rf.read()
m.render(text)

print(caputured.replacers)
print("--")
for rx, rep in caputured.replacers:
    text = rx.subn(rep, text)[0]
print(text)
