from prompt_toolkit import prompt
from prompt_toolkit.completion import WordCompleter

completer = WordCompleter(["foo", "bar", "boo"])
text = prompt("Give me some input:", completer=completer)
print(f"You said: {text}")
