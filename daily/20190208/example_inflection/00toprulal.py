import inflection


def f(name):
    return f"{name} -> {inflection.pluralize(name)}"


print(f("item"))
print(f("person"))
