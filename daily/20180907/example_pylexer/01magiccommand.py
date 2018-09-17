from IPython.lib.lexers import IPython3Lexer

code = """
%matplotlib inline
"""
lexer = IPython3Lexer()
for token in lexer.get_tokens(code.strip()):
    print(token)

# (Token.Operator, '%')
# (Token.Keyword, 'matplotlib')
# (Token.Text, ' inline\n')
