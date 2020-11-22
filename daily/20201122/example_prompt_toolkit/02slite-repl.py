from __future__ import unicode_literals
from prompt_toolkit import PromptSession
from prompt_toolkit.lexers import PygmentsLexer
from pygments.lexers.sql import SqlLexer


def main():
    session = PromptSession(lexer=PygmentsLexer(SqlLexer))

    while True:
        try:
            text = session.prompt("> ")
        except KeyboardInterrupt:
            continue
        except EOFError:
            break
        else:
            print("You entered:", text)
    print("GoodBye!")


if __name__ == "__main__":
    main()
