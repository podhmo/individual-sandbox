from prompt_toolkit import PromptSession
from prompt_toolkit.completion import WordCompleter
from prompt_toolkit.lexers import PygmentsLexer
from pygments.lexers.shell import BashLexer

my_completer = WordCompleter(["--name", "--data"])


def main():
    session = PromptSession(lexer=PygmentsLexer(BashLexer), completer=my_completer)

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
