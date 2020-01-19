from shosai.parsing import parse_title
from mistune import Markdown

title = """[foo][foo > bar]title"""
print(parse_title(title))

print(Markdown().inline(title))
