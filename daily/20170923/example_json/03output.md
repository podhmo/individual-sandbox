``` python
import abc
import extjson


class HasName(abc.ABC):
    pass


class Model:
    def __name__(self):
        return self.__class__.__name__


class A(Model):
    pass


class B(Model):
    pass


@extjson.register(HasName)
def dump_name(o):
    return o.__name__()


HasName.register(Model)

print(extjson.dumps(B()))  # B

# "B"
```
