from pyparsing import alphanums

IDENT_CHAR = alphanums + "@_$"


def make(IDENT_CHAR=IDENT_CHAR):
    from moz_sql_parser.keywords import RESERVED
    from pyparsing import (
        Combine,
        Forward,
        Group,
        Keyword,
        Literal,
        Optional,
        ParserElement,
        Regex,
        Word,
        ZeroOrMore,
        alphanums,
        delimitedList,
    )

    def to_string(instring, tokensStart, retTokens):
        val = retTokens[0]
        val = "'" + val[1:-1].replace("''", "\\'") + "'"
        return {"literal": ast.literal_eval(val)}

    def unquote(instring, tokensStart, retTokens):
        val = retTokens[0]
        if val.startswith("'") and val.endswith("'"):
            val = "'" + val[1:-1].replace("''", "\\'") + "'"
            # val = val.replace(".", "\\.")
        elif val.startswith('"') and val.endswith('"'):
            val = '"' + val[1:-1].replace('""', '\\"') + '"'
            # val = val.replace(".", "\\.")
        elif val.startswith("`") and val.endswith("`"):
            val = '"' + val[1:-1].replace("``", "`") + '"'
        elif val.startswith("+"):
            val = val[1:]
        un = ast.literal_eval(val)
        return un

    sqlString = Regex(r"\'(\'\'|\\.|[^'])*\'").addParseAction(to_string)
    identString = Regex(r'\"(\"\"|\\.|[^"])*\"').addParseAction(unquote)
    mysqlidentString = Regex(r"\`(\`\`|\\.|[^`])*\`").addParseAction(unquote)
    ident = Combine(
        ~RESERVED
        + (
            delimitedList(
                Literal("*") | identString | mysqlidentString | Word(IDENT_CHAR),
                delim=".",
                combine=True,
            )
        )
    ).setName("identifier")
    return ident


words = ["foo", "foo.bar", "foo_bar", "foo@bar", ":foo"]

ident = make()
for word in words:
    try:
        print("<-", word)
        print("->", ident.parseString(word))
    except Exception as e:
        print("!?", e)

print("----------------------------------------")

ident = make(IDENT_CHAR + ":")
for word in words:
    try:
        print("<-", word)
        print("->", ident.parseString(word))
    except Exception as e:
        print("!?", e)
