from prompt_toolkit import prompt
from prompt_toolkit.contrib.completers import WordCompleter

html_completer = WordCompleter(['<html>', '<body>', '<head>', '<title>'])
text = prompt('Enter HTML: ', completer=html_completer)
print('You said: ', text)
