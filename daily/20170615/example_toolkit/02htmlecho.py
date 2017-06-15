from prompt_toolkit.shortcuts import prompt
from prompt_toolkit.styles import style_from_pygments
from prompt_toolkit.layout.lexers import PygmentsLexer

from pygments.styles.tango import TangoStyle
from pygments.token import Token
from pygments.lexers import HtmlLexer

our_style = style_from_pygments(
    TangoStyle, {
        Token.Comment: '#888888 bold',
        Token.Keyword: '#ff88ff bold',
    }
)

text = prompt('Enter HTML: ', lexer=PygmentsLexer(HtmlLexer), style=our_style)
print('You said: {}'.format(text))
