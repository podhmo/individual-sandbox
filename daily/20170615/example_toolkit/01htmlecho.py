from pygments.lexers import HtmlLexer
from prompt_toolkit.shortcuts import prompt
from prompt_toolkit.layout.lexers import PygmentsLexer

text = prompt('Enter HTML', lexer=PygmentsLexer(HtmlLexer))
print('You said: {}'.format(text))
