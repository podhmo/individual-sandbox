import os.path
from collections import (
    OrderedDict,
    defaultdict,
)
from prestring import LazyFormat
from prestring.go import GoModule


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
        self.files = OrderedDict()

    def string(self):
        return "package {}".format(self.name)

    def verbose(self):
        return self.string()

    def __getattr__(self, name):
        v = self.symbol(name)
        setattr(self, name, v)
        return v

    @property
    def filepath(self):
        if self.virtual:
            return ""
        return os.path.join(os.getenv("GOPATH"), "src", self.fullname)

    def import_(self, fullname, name=None):
        return self.new_instance(ImportedPackage, fullname, name, package=self)

    def file(self, name):
        if name not in self.files:
            file = self.new_instance(File, name, package=self)
            self.files[name] = file
        return self.files[name]

    def type(self, name):
        return self.new_instance(Type, name, package=self)

    def symbol(self, name):
        return self.new_instance(Symbol, name, package=self)


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
    def fullname(self):
        return os.path.join(self.package.filepath, self.name)

    def import_(self, fullname, as_=None):
        if fullname in self.imported:
            return self.imported[fullname]
        v = self.imported[fullname] = self.package.import_(fullname, name=as_)
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
    def __init__(self, fullname, name=None, package=None):
        self.fullname = fullname
        self.name = nameof(fullname, name)
        self.as_ = name
        self.package = package
        self.virtual = False

    def string(self):
        return self.fullname

    def verbose(self):
        return self.string()

    def type(self, name):
        return Type(name, package=self)

    def __getattr__(self, name):
        v = self.symbol(name)
        setattr(self, name, v)
        return v

    def symbol(self, name):
        return self.new_instance(Symbol, name, package=self)


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
class Symbol(Type):
    def __init__(self, name, package):
        self.name = name
        self.package = package

    def __call__(self, *args):
        return LazyFormat("{}({})", self.string(), ", ".join([_encode(e) for e in args]))


def _encode(v):
    if isinstance(v, (str, bytes)):
        return '"{}"'.format(v)
    else:
        return str(v)


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
                setattr(self, name, e)
                return e
        for e in self._returns or []:
            if e.name == name:
                setattr(self, name, e)
                return e
        raise AttributeError(name)

    def __enter__(self):
        return self

    def __exit__(self, type, value, tb):
        return None

    def args(self, *args):
        self._args = self.new_instance(Args, args, function=self)  # xxx
        return self

    def returns(self, *args):
        self._returns = self.new_instance(Returns, args, function=self)  # xxx
        return self

    def body(self, fn):
        self._body = fn  # xxx
        return self

    def __call__(self, *args):
        return LazyFormat("{}({})", self.name, ", ".join([_encode(e) for e in args]))

    def string(self):
        args = "" if self._args is None else self._args.withtype(self.file)
        returns = "" if self._returns is None else " {}".format(self._returns.withtype(self.file))
        return "func {}({}) {}".format(self.name, args, returns)

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
    def __init__(self, module_factory=GoModule):
        self.modules = defaultdict(module_factory)  # module is prestring's module

    def create_import_function(self, im, file):
        def import_(fullname, as_=None):
            im(fullname, as_=as_)
            return file.import_(fullname, as_=as_)

        return import_

    def _write_import_part(self, file, m):
        with m.import_group() as im:
            m.import_ = self.create_import_function(im, file)
            for ipackage in file.imported.values():
                im(ipackage.fullname, as_=ipackage.as_)

    def _write_function_part(self, file, m):
        for func in file.functions.values():
            m.append(str(func))
            m.stmt(" {")
            with m.scope():
                func._body(m)
            m.stmt("}")
            m.sep()

    def write_file(self, file, m=None):
        m = m or self.modules[file.fullname]
        m.stmt(str(file.package))
        m.sep()
        self._write_import_part(file, m)
        self._write_function_part(file, m)
        return m


class LazyString(object):
    def __init__(self, v):
        self.v = v

    def __getattr__(self, name):
        return getattr(self.v, name)

    def __str__(self):
        return str(self)


def get_repository(stringer=None, writer=None):
    stringer = stringer or Stringer()
    writer = writer or Writer()
    return Repository(stringer, writer)


class Repository:
    # todo: writer
    def __init__(self, stringer, writer):
        self.stringer = stringer
        self.writer = writer
        self.builtins = self.make_builtins()
        self.packages = OrderedDict()

    def __getattr__(self, name):
        return getattr(self.builtins, name)

    def make_builtins(self):
        b = Package("*builtins*", name="*builtins*", virtual=True, repository=self)
        b.int = b.type("int")
        b.string = b.type("string")
        return b

    def package(self, fullname, name=None):
        if fullname not in self.packages:
            package = Package(fullname, name=name, repository=self)
            self.packages[fullname] = package
        return self.packages[fullname]
