from parsimonious.grammar import Grammar

grammar = Grammar(
    r"""
    sentence = styled_text+
    styled_text = bold_text / italic_text / text
    bold_text   = "((" text "))"
    italic_text = "''" text "''"
    text        = ~"[A-Z 0-9]+"i
"""
)

parsed = grammar.parse("((bold stuff)) foo ''italic''")
# parsed = grammar.parse("''((bold stuff)) foo'' normal")
print(parsed)
