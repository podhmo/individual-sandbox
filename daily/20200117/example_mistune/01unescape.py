from mistune import Markdown
import html

title = """[foo][foo > bar]title"""

m = Markdown()
print(m.inline(title))
print(html.unescape(m.inline(title)))
