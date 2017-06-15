from prompt_toolkit import prompt
from prompt_toolkit.history import InMemoryHistory
from prompt_toolkit.auto_suggest import AutoSuggestFromHistory

history = InMemoryHistory()
for word in ["apple", "banana", "orange", "grape", "pineapple"]:
    history.append(word)

while True:
    text = prompt('> ', history=history, auto_suggest=AutoSuggestFromHistory())
    print('You said: ', text)
