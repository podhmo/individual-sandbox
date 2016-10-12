from models import Foo


def foo_service1():
    foo = Foo(name="foo name")
    foo.desc = "foo desc"
    foo.save()

def foo_service2():
    foo = Foo(desc="foo desc")
    foo.name = "foo name"
    foo.save()

def foo_service3():
    foo = Foo()
    foo.name = "foo name"
    foo.desc = "foo desc"
    foo.save()
