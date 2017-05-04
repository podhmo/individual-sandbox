import os.path
from collections import OrderedDict


class reify(object):
    def __init__(self, wrapped):
        self.wrapped = wrapped
        try:
            self.__doc__ = wrapped.__doc__
        except:
            pass

    def __get__(self, inst, objtype=None):
        if inst is None:
            return self
        val = self.wrapped(inst)
        setattr(inst, self.wrapped.__name__, val)
        return val


class Stringer:
    def string(self, value):
        return value.string()


class VerboseStringer:
    def string(self, value):
        if hasattr(value, "verbose"):
            return value.verbose()
        else:
            return "<{value.__class__.__module__}.{value.__class__.__name__}: {!r}>".format(
                value.string(), value=value
            )


class FullnameStringer:
    def string(self, value):
        return value.fullname


class Repository:
    # todo: writer
    def __init__(self, stringer):
        self.stringer = stringer
        self.builtins = self.make_builtins()

    def __getattr__(self, name):
        return getattr(self.builtins, name)

    def make_builtins(self):
        b = Package("*builtins*", name="*builtins*", virtual=True, repository=self)
        b.int = b.type("int")
        b.string = b.type("string")
        return b

    def package(self, fullname, name=None):
        return Package(fullname, name=name, repository=self)


def nameof(fullname, name=None):
    return name or fullname.split("/", -1)[-1]


def stringable(cls):  # why not using inheritance?
    if "_stringable" in cls.__dict__:
        return cls
    # marked
    cls._stringable = True

    original = cls.__init__

    def init(self, *args, repository=None, **kwargs):
        original(self, *args, **kwargs)
        self.repository = repository

    def __str__(self):
        return self.repository.stringer.string(self)

    cls.__str__ = __str__
    cls.__init__ = init

    if not hasattr(cls, "string"):
        raise NotImplementedError("string() method is not found")
    return cls


class Typeaable:
    def value(self, name):
        return Value(name, type=self, repository=self.repository)

    def typename(self, file, typename=None):
        if file.package.fullname == self.package.fullname:
            name = self.name
        else:
            name = "{}.{}".format(self.package.name, self.name)
        if typename is not None:
            name = typename.replace(self.name, name)
        return name

    __call__ = value


class Valueable:
    @reify
    def ref(self):
        return Ref(self, repository=self.repository)

    @reify
    def pointer(self):
        return Pointer(self, repository=self.repository)

    @reify
    def slice(self):
        return Slice(self, repository=self.repository)

    def as_argument(self, file):
        return "{} {}".format(self.name, self.typename(file))


@stringable
class Package:
    def __init__(self, fullname, name=None, virtual=False):
        self.fullname = fullname
        self.name = nameof(fullname, name)
        self.virtual = virtual

    def string(self):
        return "package {}".format(self.name)

    @property
    def filepath(self):
        if self.virtual:
            return ""
        return os.path.join(os.getenv("GOPATH"), "src", self.fullname)

    def import_(self, fullname, name=None):
        return ImportedPackage(fullname, name, repository=self.repository)

    def file(self, name):
        return File(name, package=self, repository=self.repository)

    def type(self, name):
        return Type(name, package=self, repository=self.repository)


@stringable
class File:
    def __init__(self, name, package):
        self.name = name
        self.package = package
        self.imported = OrderedDict()

    def string(self):
        return self.filepath

    @property
    def filepath(self):
        return os.path.join(self.package.filepath, self.name)

    def import_(self, fullname, name=None):
        if fullname in self.imported:
            return self.imported[fullname]
        v = self.imported[fullname] = self.package.import_(fullname, name=name)
        return v

    def enum(self, name, type, comment=None):
        return Enum(name, file=self, type=type, comment=comment, repository=self.repository)


@stringable
class Enum(Typeaable):
    def __init__(self, name, type, file, comment=None):
        self.name = name
        self.type = type
        self.file = file
        self.comment = comment
        self.members = OrderedDict()

    def __enter__(self):
        return self.add_member

    def __exit__(self, type, value, tb):
        return None

    def string(self):
        if self.package.virtual:
            return self.name
        return "{}.{}".format(self.package.name, self.name)

    def verbose(self):
        return "<{}.{} name='{}.{}', type='{}', members={!r}>".format(
            self.__class__.__module__, self.__class__.__name__, self.file.package.name, self.name,
            self.type.string(), dict(self.members)
        )

    def add_member(self, name, value, comment=None):
        member = (name, value, comment)
        self.members[name] = member
        return member

    # typeable
    @property
    def package(self):
        return self.file.package


@stringable
class ImportedPackage:
    def __init__(self, fullname, name=None):
        self.fullname = fullname
        self.name = nameof(fullname, name)

    def string(self):
        return self.fullname

    def type(self, name):
        return Type(name, package=self)


@stringable
class Type(Typeaable):
    def __init__(self, name, package):
        self.name = name
        self.package = package

    def string(self):
        if self.package.virtual:
            return self.name
        return "{}.{}".format(self.package.name, self.name)

    @property
    def fullname(self):
        return "{}.{}".format(self.package.fullname, self.name)


class Function:
    def __init__(self, name, file, args, returns=None):
        self.name = name
        self.file = file
        self.args = args
        self.returns = returns


@stringable
class Value(Valueable):
    """
    func f(<x int>)
    func f(<x foo.bar>)
    x += 1
    """

    def __init__(self, name, type):
        self.name = name
        self.type = type

    def string(self):
        return self.name

    def verbose(self):
        return "<{}.{} name='{}', type='{}'>".format(
            self.__class__.__module__, self.__class__.__name__, self.name, self.type.string()
        )

    @property
    def fullname(self):
        return "{}.{}".format(self.type.fullname, self.name)

    def typename(self, file):
        return self.type.typename(file)

    @property
    def package(self):
        return self.type.package


@stringable
class Ref(Valueable):
    def __init__(self, v):
        self.v = v

    def string(self):
        return "&{}".format(self.v.string())

    verbose = string

    def __getattr__(self, name):
        return getattr(self.v, name)

    @property
    def deref(self):
        return self.v

    depointer = deref

    def typename(self, file):
        return "&{}".format(self.v.typename(file))


@stringable
class Pointer(Valueable):
    def __init__(self, v):
        self.v = v

    def string(self):
        return "*{}".format(self.v.string())

    verbose = string

    def __getattr__(self, name):
        return getattr(self.v, name)

    @property
    def depointer(self):
        return self.v

    depointer = depointer

    def typename(self, file):
        return "*{}".format(self.v.typename(file))


@stringable
class Slice(Valueable):
    def __init__(self, v):
        self.v = v

    def string(self):
        return "[]{}".format(self.v.string())

    verbose = string

    def __getattr__(self, name):
        return getattr(self.v, name)

    def typename(self, file):
        return "[]{}".format(self.v.typename(file))

    def as_argument(self, file, typename=None):
        return self.v.as_argument(file=file, typename=typename or self.typename(file))


def get_repository(stringer=Stringer()):
    return Repository(stringer)
