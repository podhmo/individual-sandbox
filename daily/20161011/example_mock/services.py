from models import Foo


def foo_service():
    foo = Foo("name")
    foo.desc = "desc"
    foo.save()
    return foo
