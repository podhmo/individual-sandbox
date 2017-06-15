from prompt_toolkit.shortcuts import print_tokens
from prompt_toolkit.styles import style_from_dict
from pygments.token import Token

# Create a stylesheet.
style = style_from_dict({
    Token.Hello: '#ff0066',
    Token.World: '#44ff44 italic',
})

# Make a list of (Token, text) tuples.
tokens = [
    (Token.Hello, 'Hello '),
    (Token.World, 'World'),
    (Token, '\n'),
]

# Print the result.
print_tokens(tokens, style=style)
