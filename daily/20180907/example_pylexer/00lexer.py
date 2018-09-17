from pygments.lexers import Python3Lexer

code = """
def hello(name: str) -> None:
    print(f"{name}: hello")
"""

lexer = Python3Lexer()
for token in lexer.get_tokens(code.strip()):
    print(token)

# (Token.Keyword, 'def')
# (Token.Text, ' ')
# (Token.Name.Function, 'hello')
# (Token.Punctuation, '(')
# (Token.Name, 'name')
# (Token.Punctuation, ':')
# (Token.Text, ' ')
# (Token.Name.Builtin, 'str')
# (Token.Punctuation, ')')
# (Token.Text, ' ')
# (Token.Operator, '-')
# (Token.Operator, '>')
# (Token.Text, ' ')
# (Token.Keyword.Constant, 'None')
# (Token.Punctuation, ':')
# (Token.Text, '\n')
# (Token.Text, '    ')
# (Token.Name.Builtin, 'print')
# (Token.Punctuation, '(')
# (Token.Name, 'f')
# (Token.Literal.String.Double, '"')
# (Token.Literal.String.Interpol, '{name}')
# (Token.Literal.String.Double, ': hello')
# (Token.Literal.String.Double, '"')
# (Token.Punctuation, ')')
# (Token.Text, '\n')
