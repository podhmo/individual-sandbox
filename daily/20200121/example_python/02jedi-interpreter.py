import jedi
import string


def get_completion(source, namespace):
    i = jedi.Interpreter(source, [namespace])
    completions = i.completions()
    return completions


print(get_completion("string.c", locals()))
print(get_completion("string.c", globals()))
