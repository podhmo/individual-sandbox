from prompt_toolkit import prompt
from prompt_toolkit.key_binding.manager import KeyBindingManager
from prompt_toolkit.keys import Keys

manager = KeyBindingManager.for_prompt()


@manager.registry.add_binding(Keys.ControlT)
def _(event):
    def print_hello():
        print('hello world')

    event.cli.run_in_terminal(print_hello)


text = prompt('> ', key_bindings_registry=manager.registry)
print('You said: ', text)
