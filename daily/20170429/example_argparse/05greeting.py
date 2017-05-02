# greeting.py
from handofcats import as_command

@as_command
def greeting(message, is_surprised=False, name="foo"):
    suffix = "!" if is_surprised else ""
    print("{name}: {message}{suffix}".format(name=name, message=message, suffix=suffix))
