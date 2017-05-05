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

    def new_instance(self, cls, *args, **kwargs):
        kwargs["repository"] = self.repository
        return cls(*args, **kwargs)

    cls.__str__ = __str__
    cls.__init__ = init
    cls.new_instance = new_instance

    if not hasattr(cls, "string"):
        raise NotImplementedError("string() method is not found")
    return cls


class Typeaable:
    # self.name
    # self.package
    def value(self, name):
        return self.new_instance(Value, name, type=self)

    def typename(self, file, typename=None):
        if self.package.virtual or file.package.fullname == self.package.fullname:
            name = self.name
        else:
            name = "{}.{}".format(self.package.name, self.name)
        if typename is not None:
            name = typename.replace(self.name, name)
        return name

    __call__ = value


class Valueable:
    # self.name
    @reify
    def ref(self):
        return self.new_instance(Ref, self)

    @reify
    def pointer(self):
        return self.new_instance(Pointer, self)

    @reify
    def slice(self):
        return self.new_instance(Slice, self)

    def withtype(self, file):
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
        return self.new_instance(ImportedPackage, fullname, name)

    def file(self, name):
        return self.new_instance(File, name, package=self)

    def type(self, name):
        return self.new_instance(Type, name, package=self)


@stringable
class File:
    def __init__(self, name, package):
        self.name = name
        self.package = package
        self.imported = OrderedDict()
        self.functions = OrderedDict()
        self.enums = OrderedDict()

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
        enum = self.new_instance(Enum, name, file=self, type=type, comment=comment)
        self.enums[name] = enum
        return enum

    def func(self, name, comment=None):
        function = self.new_instance(Function, name, file=self, comment=comment)
        self.functions[name] = function
        return function


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


@stringable
class Function(Valueable):
    def __init__(self, name, file, args=None, returns=None, body=None, comment=None):
        self.name = name
        self.file = file
        self._args = args
        self._returns = returns
        self._body = body
        self.comment = comment

    def __getattr__(self, name):
        for e in self._args or []:
            if e.name == name:
                return e
        for e in self._returns or []:
            if e.name == name:
                return e
        raise AttributeError(name)

    def __call__(self, fn):
        return fn

    def args(self, *args):
        args = self.new_instance(Args, args, function=self)
        return self.new_instance(
            self.__class__,
            name=self.name,
            file=self.file,
            args=args,
            returns=self._returns,
            body=self._body,
            comment=self.comment,
        )

    def returns(self, *args):
        returns = self.new_instance(Returns, args, function=self)
        return self.new_instance(
            self.__class__,
            name=self.name,
            file=self.file,
            args=self._args,
            returns=returns,
            body=self._body,
            comment=self.comment,
        )

    def body(self, fn):
        return self.new_instance(
            self.__class__,
            name=self.name,
            file=self.file,
            args=self._args,
            returns=self._returns,
            body=fn,
            comment=self.comment,
        )

    # todo: writer
    def __call__(self, m):
        m.append(self.string())
        m.stmt(" {")
        with m.scope():
            self._body(m)
        m.stmt("}")

    def string(self):
        args = "" if self._args is None else self._args.withtype(self.file)
        returns = "" if self._returns is None else " {}".format(self._returns.withtype(self.file))
        return "func {}({}) {}".format(self.name, self._args, self._returns)

    def typename(self, file):
        args = "" if self._args is None else self._args.typename(file)
        returns = "" if self._returns is None else " {}".format(self._returns.typename(file))
        return "func({}){}".format(args, returns)


@stringable
class Args:
    def __init__(self, args, function):
        self.args = args
        self.function = function

    def __iter__(self):
        return iter(self.args)

    def string(self):
        return self.withtype(self.function.file)

    def typename(self, file):
        return ", ".join([e.typename(file) for e in self.args])

    def withtype(self, file):
        return ", ".join([e.withtype(file) for e in self.args])


@stringable
class Returns:
    def __init__(self, args, function):
        self.args = args
        self.function = function

    def __iter__(self):
        return iter(self.args)

    def string(self):
        return self.withtype(self.function.file)

    def typename(self, file):
        if not self.args:
            return ""
        elif len(self.args) == 1:
            return self.args[0].typename(file)
        else:
            return "({})".format(", ".join([e.typename(file) for e in self.args]))

    def withtype(self, file):
        return self.typename(file)  # xxx


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

    def withtype(self, file, typename=None):
        return self.v.withtype(file=file, typename=typename or self.typename(file))


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


class Writer:
    pass


def get_repository(stringer=Stringer(), writer=Writer()):
    return Repository(stringer, writer)


class Repository:
    # todo: writer
    def __init__(self, stringer, writer):
        self.stringer = stringer
        self.writer = writer
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
