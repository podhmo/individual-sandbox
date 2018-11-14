from prompt_toolkit import prompt, print_formatted_text

text = prompt('Give me some input: ')
print_formatted_text('You said: %s' % text)
