# need: editdistance, prestring(go branch)
import editdistance
import sys
import argparse
import json
import logging
from collections import ChainMap, deque, defaultdict, namedtuple, OrderedDict
from prestring.go import GoModule

Action = namedtuple("Action", "action, src, dst")
logger = logging.getLogger(__name__)


# stolen from pyramid
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


class Reader(object):
    def read_world(self, data, parent=None):
        world = World(parent=parent, reader=self)
        for name, module in data["module"].items():
            world.read_module(name, module)
        return world

    def read_module(self, data, parent=None):
        module = Module(data["name"], data["fullname"], parent=parent, reader=self)
        for name, file in data["file"].items():
            module.read_file(name, file)
        return module

    def read_file(self, data, parent=None):
        file = File(data["name"], data["import"], parent=parent, reader=self)
        for name, alias in data["alias"].items():
            file.read_alias(name, alias)
        for name, struct in data["struct"].items():
            file.read_struct(name, struct)
        return file

    def read_struct(self, data, parent=None):
        struct = Struct(data["name"], data, parent=parent, reader=self)
        for name, field in data["fields"].items():
            struct.read_field(name, field)
        return struct

    def read_alias(self, data, parent=None):
        alias = Alias(data["name"], data, parent=parent, reader=self)
        return alias

    def read_field(self, data, parent=None):
        field = Field(data["name"], data, parent=parent, reader=self)
        return field


class World(object):
    def __init__(self, parent=None, reader=None):
        self.parent = parent
        self.reader = reader
        self.modules = OrderedDict()
        self.modules_by_fullname = {}

    def read_module(self, name, module):
        self.modules[name] = self.reader.read_module(module, parent=self)
        self.modules_by_fullname[self.modules[name].fullname] = self.modules[name]

    def normalize(self):
        for module in self.modules.values():
            module.normalize()

    def __getitem__(self, name):
        return self.modules[name]


class Module(object):
    def __init__(self, name, fullname, parent=None, reader=None):
        self.name = name
        self.fullname = fullname
        self.parent = parent
        self.reader = reader
        self.files = OrderedDict()
        self.reset()

    @property
    def package_name(self):
        return self.name

    def fulladdress(self, name):
        return "{}.{}".format(self.fullname, name)

    def read_file(self, name, file):
        self.files[name] = self.reader.read_file(file, parent=self)

    def normalize(self):
        self.reset()
        for file in self.files.values():
            file.normalize()
            self.new_child(file.members)

    def reset(self):
        self.members = ChainMap()

    def new_child(self, item):
        self.members = self.members.new_child(item)

    def __getitem__(self, name):
        return self.members[name]

    def get(self, name, default=None):
        return self.members.get(name, default)

    def __contains__(self, name):
        return name in self.members

    @property
    def world(self):
        return self.parent


class File(object):
    def __init__(self, name, imports, parent=None, reader=None):
        self.name = name
        self.imports = imports
        self.parent = parent
        self.reader = reader
        self.aliases = OrderedDict()
        self.structs = OrderedDict()
        self.members = {}

    @property
    def package_name(self):
        if self.parent is None:
            return None
        return self.parent.package_name

    def normalize(self):
        for struct in self.structs.values():
            struct.normalize()

        for alias in self.aliases.values():
            alias.normalize()

    def read_alias(self, name, alias):
        self.members[name] = self.aliases[name] = self.reader.read_alias(alias, parent=self)

    def read_struct(self, name, struct):
        self.members[name] = self.structs[name] = self.reader.read_struct(struct, parent=self)

    def dump(self, writer):
        return writer.write_file(self)

    @property
    def world(self):
        return self.parent.parent

    @property
    def module(self):
        return self.parent


class Alias(object):
    def __init__(self, name, data, parent=None, reader=None):
        self.name = name
        self.parent = parent
        self.reader = reader
        self.data = data

    @property
    def package_name(self):
        if self.parent is None:
            return None
        return self.parent.package_name

    @property
    def src_name(self):
        return self.data["original"]["value"]

    @reify
    def src_fullname(self):
        if "prefix" not in self.data["original"]:
            return self.src_name
        module = self.module
        prefix = self.data["original"]["prefix"]
        if module.name == prefix:
            full_prefix = module.fullname
        else:
            full_prefix = self.parent.imports[prefix]["fullname"]
        return "{}.{}".format(full_prefix, self.src_name)

    @reify
    def fullname(self):
        return self.module.fulladdress(self.data["name"])

    def dump(self, writer):
        return writer.write_alias(self)

    def normalize(self):
        pass

    @property
    def world(self):
        return self.parent.parent.parent

    @property
    def module(self):
        return self.parent.parent

    @property
    def file(self):
        return self.parent


class Struct(object):
    def __init__(self, name, data, parent=None, reader=None):
        self.name = name
        self.parent = parent
        self.reader = reader
        self.data = data
        self.fields = OrderedDict()

    @property
    def package_name(self):
        if self.parent is None:
            return None
        return self.parent.package_name

    @property
    def fullname(self):
        return self.module.fulladdress(self.name)

    def read_field(self, name, field):
        field = self.reader.read_field(field, self)
        self.fields[name.lower()] = field

    def dump(self, writer):
        return writer.write_struct(self)

    def __getitem__(self, name):
        return self.fields[name]

    def __contains__(self, name):
        return name in self.fields

    def normalize(self):
        pass

    @property
    def world(self):
        return self.parent.parent.parent

    @property
    def module(self):
        return self.parent.parent

    @property
    def file(self):
        return self.parent


class Field(object):
    def __init__(self, name, data, parent=None, reader=None):
        self.name = name
        self.data = data
        self.parent = parent
        self.reader = reader

    @property
    def type(self):
        return self.data["type"]

    @reify
    def type_expr(self):
        r = []
        for x in self.type_path:
            if x == "pointer":
                r.append("*")
            elif x == "array":
                r.append("[]")
            elif "/" in x:
                r.append(x.rsplit("/", 1)[-1])
            else:
                r.append(x)
        return "".join(r)

    @reify
    def suffix(self):
        r = []
        for x in self.type_path:
            if x == "pointer":
                r.append("Ref")
            elif x == "array":
                r.append("Many")
            elif "." in x:
                r.append(x.rsplit(".", 1)[-1].title())
            else:
                r.append(x)
        return "".join(reversed(r))

    @reify
    def type_path(self):
        return tuple(self.get_type_path(self.type, self.parent))

    def get_type_path(self, value, struct):
        if value["kind"] == "primitive":
            module = struct.module
            if value["value"] not in module:
                return [value["value"]]
            else:
                return ["{}.{}".format(module.fullname, value["value"])]
        elif value["kind"] == "selector":
            prefix = struct.parent.imports[value["prefix"]]["fullname"]
            return ["{}.{}".format(prefix, value["value"])]
        else:
            r = [value["kind"]]
            r.extend(self.get_type_path(value["value"], struct))
            return r

    @reify
    def definition(self):
        if "." not in self.type_path[-1]:
            return self.type
        prefix, name = self.type_path[-1].rsplit(".", 1)
        return self.world.modules_by_fullname[prefix][name]

    def find_module(self, name):
        file = self.file
        module = file.parent
        if module.name == name:
            fullname = module.fullname
        else:
            fullname = file.imports[name]["fullname"]
        return module.parent.modules_by_fullname[fullname]

    @property
    def world(self):
        return self.parent.parent.parent.parent

    @property
    def module(self):
        return self.parent.parent.parent

    @property
    def file(self):
        return self.parent.parent

    @property
    def struct(self):
        return self.parent


class ManyConvertWriter(object):
    def __init__(self, writer):
        self.writer = writer  # ConvertWriter
        self.m = self.writer.m
        self.convertor = self.writer.convertor

    def get_function_name(self, src_field, dst_field):
        # src is array field?
        src = src_field.definition
        dst = dst_field.definition
        return 'From{}{}To{}{}'.format(src.package_name.title(), src_field.suffix, dst.package_name.title(), dst_field.suffix)

    def write_many(self, fnname, src_field, dst_field, inner, cont):
        src_arg = 'src {}'.format(src_field.type_expr)
        dst_arg = '{}'.format(dst_field.type_expr)
        self.m.comment("{} : converts {} -> {}".format(fnname, src_field.type_expr, dst_field.type_expr))
        with self.m.func(fnname, src_arg, return_="({}, error)".format(dst_arg)):
            self.m.stmt("dst := make({}, len(src))".format(dst_field.type_expr))
            with self.m.for_("i, x := range src"):
                m, convert = self.convertor.convert_from_code(self.m, inner, src_field, dst_field, name="x")
                m.stmt("dst[i] = {}".format(convert))
            self.m.return_("dst, nil")

    def register(self, fnname, src_type_list, dst_type_list):
        return self.writer.register(fnname, src_type_list, dst_type_list, skip_resolve_register=True)


class ConvertWriter(object):
    def __init__(self, m, convertor):
        self.m = m
        self.convertor = convertor
        self.many = ManyConvertWriter(self)
        self.used = set()

    def get_function_name(self, src, dst):
        return 'From{}{}To{}{}'.format(src.package_name.title(), src.name, dst.package_name.title(), dst.name)

    def write_function(self, src, dst):
        fnname = self.get_function_name(src, dst)
        src_type_list = tuple(["pointer", src.fullname])
        dst_type_list = tuple(["pointer", dst.fullname])
        cont = []
        self.register(fnname, src_type_list, dst_type_list)
        self.write(fnname, src, dst, cont)
        for fn in cont:
            fn()

    def write(self, fnname, src, dst, cont):
        if fnname in self.used:
            return
        src_arg = 'src *{}.{}'.format(src.package_name, src.name)
        dst_arg = '*{}.{}'.format(dst.package_name, dst.name)
        self.m.comment("{} : converts {}.{} -> {}.{}".format(fnname, src.package_name, src.name, dst.package_name, dst.name))
        with self.m.func(fnname, src_arg, return_="({}, error)".format(dst_arg)):
            with self.m.if_("src == nil"):
                self.m.return_("nil, nil")
            self.m.stmt("dst := &{}.{}{{}}".format(dst.package_name, dst.name))
            for name, field in sorted(dst.fields.items()):
                self.write_code_convert(src, dst, name, field, cont)
            self.m.return_("dst, nil")
        self.used.add(fnname)

    def register(self, fnname, src_type_list, dst_type_list, skip_resolve_register=False):
        override = self.convertor.as_override

        @override(src_type_list, dst_type_list, skip_resolve_register=skip_resolve_register)
        def subconvert(convertor, m, value, *args):
            tmp = convertor.gensym()
            m.stmt("{}, err := {}({})".format(tmp, fnname, value))
            with m.if_("err != nil"):
                m.return_("nil, err")
            return tmp

    def write_code_convert(self, src, dst, name, field, cont, retry=None):
        try:
            if name in src:
                src_field = src[name]
                value = "src.{}".format(src_field.name)
                m, convert = self.convertor.convert(self.m, src_field, field, name=value)
                m.stmt("dst.{} = {}".format(field.name, convert))  # xxx
            else:
                try:
                    score, nearlest = min([(editdistance.eval(name, f.name), f.name) for f in src.fields.values()])
                    self.m.comment("FIXME: {} is not found. (maybe {}?)".format(field.name, nearlest))
                except ValueError:
                    self.m.comment("FIXME: {} is not found.".format(field.name))
        except GencodeMappingManyNotFound as e:
            if retry and isinstance(retry, e.__class__):
                raise
            return self._fallback_for_many(e, src, dst, name, field, cont)
        except GencodeMappingNotFound as e:
            if retry and isinstance(retry, e.__class__):
                raise
            return self._fallback(e, src, dst, name, field, cont)

    def _fallback(self, e, src, dst, name, field, cont):
        if e.src_path[-1].endswith(("32", "64")) or e.dst_path[-1].endswith(("32", "64")):
            primitive_src = e.src_path[-1].replace("32", "").replace("64", "")
            primitive_dst = e.dst_path[-1].replace("32", "").replace("64", "")
            if primitive_src == primitive_dst:
                self.convertor.codegen.resolver.add_relation(e.src_path[-1], e.dst_path[-1])
                return self.write_code_convert(src, dst, name, field, cont, retry=e)

        dep_src = src[name].definition
        dep_dst = field.definition
        fnname = self.get_function_name(dep_src, dep_dst)
        # if array? don't registe raw type expression, such as `array pinter X`
        self.register(fnname, tuple(["pointer", dep_src.fullname]), tuple(["pointer", dep_dst.fullname]))
        cont.append(lambda: self.write(fnname, dep_src, dep_dst, cont))
        return self.write_code_convert(src, dst, name, field, cont, retry=e)

    def _fallback_for_many(self, e, src, dst, name, field, cont):
        if e.src_path[-1].endswith(("32", "64")) or e.dst_path[-1].endswith(("32", "64")):
            primitive_src = e.src_path[-1].replace("32", "").replace("64", "")
            primitive_dst = e.dst_path[-1].replace("32", "").replace("64", "")
            if primitive_src == primitive_dst:
                self.convertor.codegen.resolver.add_relation(e.src_path[-1], e.dst_path[-1])

        fnname = self.many.get_function_name(src[name], field)
        self.many.register(fnname, src[name].type_path, field.type_path)
        cont.append(lambda: self.many.write_many(fnname, src[name], field, e.inner, cont))
        return self.write_code_convert(src, dst, name, field, cont, retry=e)


class ImportWriter(object):
    def __init__(self, im):
        self.im = im
        self.prefix_map = {}  # fullname -> prefix
        self.name_map = {}  # name -> fullname
        self.used = set()
        self.i = 0

    def _get_prefix(self, module):
        fullname = self.name_map.get(module.name)
        if fullname is None:
            self.name_map[module.name] = module.fullname
            return module.name
        elif fullname == module.fullname:
            return module.name
        else:
            prefix = self.prefix_map.get(module.fullname)
            if prefix is None:
                prefix = self.prefix_map[module.fullname] = "{}{}".format(module.name, self.i)
                self.i += 1
            return prefix

    def import_(self, module):
        prefix = self._get_prefix(module)
        if module.fullname in self.used:
            return prefix
        self.used.add(module.fullname)
        self.im.import_(module.fullname, as_=prefix)
        return prefix


# import collections
# class LoggingMap(collections.UserDict):
#     def __setitem__(self, k, v):
#         print("logging:", k, v, file=sys.stderr)
#         super().__setitem__(k, v)


class TypeConvertor(object):
    def __init__(self, import_writer, codegen, src_world, dst_world):
        self.import_writer = import_writer
        self.codegen = codegen
        self.src_world = src_world
        self.dst_world = dst_world
        self.i = 0
        items = []
        for world, other_world in [(src_world, dst_world), (dst_world, src_world)]:
            for _, module in world.modules.items():
                for _, file in module.files.items():
                    for _, alias in file.aliases.items():
                        items.append((alias.src_fullname, alias.fullname))
        self.codegen.resolver.add_relation_list(items)
        self.override_map = {}

    def as_override(self, src_type, dst_type, skip_resolve_register=False):
        def decorator(on_write):
            self.override(src_type, dst_type, on_write, skip_resolve_register=skip_resolve_register)
            return on_write
        return decorator

    def override(self, src_type, dst_type, on_write, skip_resolve_register=False):
        if not skip_resolve_register:
            self.codegen.resolver.add_relation(src_type, dst_type)
        self.override_map[(src_type, dst_type)] = on_write

    def coerce(self, m, value, src_type, dst_type, src_field, dst_field):
        pair = (src_type, dst_type)
        if pair in self.override_map:
            return self.override_map[pair](self, m, value, src_type, dst_type, src_field, dst_field)

        if isinstance(dst_type, (list, tuple)):
            # xxx:
            return "({})({})".format("".join("*" if x == "pointer" else x for x in dst_type), value)
        elif "/" in dst_type:
            prefix_and_name = dst_type.rsplit("/", 1)[-1]
            prefix, name = prefix_and_name.rsplit(".")
            # writing import clause if needed
            new_prefix = self.import_writer.import_(dst_field.find_module(prefix))
            return "{}.{}({})".format(new_prefix, name, value)
        else:
            return "{}({})".format(dst_type, value)

    def iterate(self, m, value, code, src_field, dst_field):
        src_type = src_field.type_path
        dst_type = dst_field.type_path
        pair = (src_type, dst_type)
        if pair in self.override_map:
            return self.override_map[pair](self, m, value, src_type, dst_type, src_field, dst_field, code)
        else:
            raise GencodeMappingManyNotFound("retry for iteration {} -> {}".format(src_type, dst_type), src_type, dst_type, code)

    def gensym(self):
        self.i += 1
        return "tmp{}".format(self.i)

    def convert(self, m, src_field, dst_field, name):
        src_path = src_field.type_path
        dst_path = dst_field.type_path
        code = self.codegen.gencode(src_path, dst_path)
        return self.convert_from_code(m, code, src_field, dst_field, name)

    def convert_from_code(self, m, code, src_field, dst_field, name):
        value = name
        is_cast = False
        # print("##", name, code, file=sys.stderr)

        # optimization: x -> y; y -> z => z(x)
        coerce_buf = []

        def consume_buf(value):
            if not coerce_buf:
                return value
            value = self.coerce(m, value, *coerce_buf[-1])
            tmp = self.gensym()
            m.stmt("{} := {}".format(tmp, value))
            coerce_buf.clear()
            return tmp

        for i, op in enumerate(code):
            if is_cast:
                tmp = self.gensym()
                m.stmt("{} := {}".format(tmp, value))
                value = tmp
                is_cast = False

            if op[0] == "deref":
                value = consume_buf(value)
                with m.if_("{} != nil".format(value)):
                    value = "*({})".format(value)
                    tmp = self.gensym()
                    m.stmt("{} := {}".format(tmp, value))
                    return self.convert_from_code(m.submodule(newline=False), code[i + 1:], src_field, dst_field, tmp)
            elif op[0] == "ref":
                value = consume_buf(value)
                value = "&({})".format(value)
                is_cast = True
            elif op[0] == "coerce":
                pair = (op[1], op[2])
                if pair not in self.override_map:
                    coerce_buf.append((op[1], op[2], src_field, dst_field))
                    continue
                value = consume_buf(value)
                value = self.coerce(m, value, op[1], op[2], src_field, dst_field)
                is_cast = True
            elif op[0] == "iterate":
                value = consume_buf(value)
                # todo: deep nested
                value = self.iterate(m, value, op[1:], src_field, dst_field)
                is_cast = True
            else:
                value = consume_buf(value)
                m.comment("hmm {}".format(op[0]))
                # raise Exception(op[0])
        return m, consume_buf(value)


def _wrap_value(v):
    if isinstance(v, (tuple, list)):
        return tuple(v)
    else:
        return (v,)


def _unwrap_value(v):
    if not _has_detail(v):
        return v[0]
    else:
        return v


def _has_detail(wv):
    return len(wv) > 1


class TypeMappingResolver(object):
    def __init__(self, items):
        self.primitive_map = defaultdict(list)
        self.detail_map = defaultdict(list)
        self.add_relation_list(items)

    def add_relation_list(self, relations):
        for k, v in relations:
            self.add_relation(k, v)

    def add_relation(self, k, v):
        k = _wrap_value(k)
        v = _wrap_value(v)
        self._add_value(k, v)
        self._add_value(v, k)

    def _add_value(self, k, v):
        self.detail_map[k].append(v)
        if _has_detail(k):
            self.primitive_map[k[-1:]].append((k, v))
            self.primitive_map[v[-1:]].append((v, k))

    def resolve(self, src, dst):
        if src == dst:
            return []
        src = _wrap_value(src)
        dst = _wrap_value(dst)
        if src[-1:] == dst[-1:]:
            return self.on_finish([src], [dst])
        else:
            # print("detail_map=", self.detail_map)
            # print("primitive_map=", self.primitive_map)
            return self.tick_src([src], [dst], set(), set(), deque(), deque())

    def on_finish(self, src_hist, dst_hist):
        src_hist.extend(reversed(dst_hist))
        path = src_hist
        coerce_path = []
        for i in range(len(path) - 1):
            if path[i] == path[i + 1]:
                continue
            prev, next_ = path[i], path[i + 1]
            if coerce_path and coerce_path[-1][1] == next_ and coerce_path[-1][2] == prev:
                coerce_path.pop()
                continue
            coerce_path.append(Action(action="coerce", src=prev, dst=next_))
        return coerce_path

    def tick_src(self, src_hist, dst_hist, src_arrived, dst_arrived, src_q, dst_q):
        # print("@S", "src_hist=", src_hist, "dst_hist=", dst_hist, "src_arrived=", src_arrived, "src_q=", src_q, "dst_q=", dst_q)
        if src_hist[-1][-1] == dst_hist[-1][-1]:
            if src_hist[-1] == dst_hist[-1]:
                return self.on_finish(src_hist, dst_hist[:-1])
            else:
                return self.on_finish(src_hist, dst_hist)
        if src_hist[-1] not in src_arrived:
            src_arrived.add(src_hist[-1])
            if src_hist[-1] in self.detail_map:
                for item in self.detail_map[src_hist[-1]]:
                    src_q.append(([*src_hist, item], dst_hist))

            for i in range(1, len(src_hist[-1])):
                k = src_hist[-1][i:]
                if k in self.detail_map:
                    for item in self.detail_map[k]:
                        src_q.append(([*src_hist, k, item], dst_hist))
            k = src_hist[-1][-1:]
            if k in self.primitive_map:
                for items in self.primitive_map[k]:
                    src_q.append(([*src_hist, *items], dst_hist))
        if dst_q:
            src_hist, dst_hist = dst_q.pop()
            return self.tick_dst(src_hist, dst_hist, src_arrived, dst_arrived, src_q, dst_q)
        elif src_q:
            return self.tick_dst(src_hist, dst_hist, src_arrived, dst_arrived, src_q, dst_q)
        else:
            return None

    def tick_dst(self, src_hist, dst_hist, src_arrived, dst_arrived, src_q, dst_q):
        # print("@D", "src_hist=", src_hist, "dst_hist=", dst_hist, "src_arrived=", src_arrived, "src_q=", src_q, "dst_q=", dst_q)
        if src_hist[-1][-1] == dst_hist[-1][-1]:
            if src_hist[-1] == dst_hist[-1]:
                return self.on_finish(src_hist, dst_hist[:-1])
            else:
                return self.on_finish(src_hist, dst_hist)
        if dst_hist[-1] not in dst_arrived:
            dst_arrived.add(dst_hist[-1])
            if dst_hist[-1] in self.detail_map:
                for item in self.detail_map[dst_hist[-1]]:
                    dst_q.append((src_hist, [*dst_hist, item]))

            for i in range(1, len(dst_hist[-1])):
                k = dst_hist[-1][i:]
                if k in self.detail_map:
                    for item in self.detail_map[k]:
                        dst_q.append((src_hist, [*dst_hist, k, item]))
            k = dst_hist[-1][-1:]
            if k in self.primitive_map:
                for items in self.primitive_map[k]:
                    dst_q.append((src_hist, [*dst_hist, *items]))
            return self.tick_src(src_hist, dst_hist, src_arrived, dst_arrived, src_q, dst_q)
        if src_q:
            src_hist, dst_hist = src_q.pop()
            return self.tick_src(src_hist, dst_hist, src_arrived, dst_arrived, src_q, dst_q)
        elif dst_q:
            return self.tick_src(src_hist, dst_hist, src_arrived, dst_arrived, src_q, dst_q)
        else:
            return None


class GencodeMappingNotFound(ValueError):
    def __init__(self, msg, src, dst):
        super().__init__(msg)
        self.src_path = src
        self.dst_path = dst


class GencodeMappingManyNotFound(GencodeMappingNotFound):
    def __init__(self, msg, src, dst, args):
        super().__init__(msg, src, dst)
        self.inner = args


class MiniCodeNormalizer(object):
    def pre_gencode(self, mapping_path):
        # normalize mapping_path
        # e.g. (coerce (array x) (array y))
        #         -> (coerce (array x) x), (coerce x y), (coerce y (array y))
        # e.g. (coerce (array array x) (array array y))
        #         -> (coerce (array array x) (array x)), (coerce (array x) x), (coerce x y)(coerce y (array y)), (coerce (array y) (array array y))
        # normalized = []
        # for action in mapping_path:
        #     if action.action == "coerce":
        #         if "array" in action.src:
        #             # (p array p x) -> (coerce (p array p x) (array p x)) (coerce (array p x) (pointer x))
        #             pre_array, post_array = [], []
        unfolded = []
        for action in mapping_path:
            if action[0] != "coerce":
                unfolded.append(action)
                continue

            # src transform
            src = action[1]
            i = 0
            sub_indices = [0]
            src_tmp = []
            for x in src:
                i += 1
                if x == "array":
                    if src[sub_indices[-1]] == "array":
                        src_tmp.append(Action(action="coerce", src=src[sub_indices[-1]:], dst=src[i:]))
                    else:
                        src_tmp.append(Action(action="coerce", src=src[sub_indices[-1]:], dst=src[i - 1:]))
                        src_tmp.append(Action(action="coerce", src=src[i - 1:], dst=src[i:]))
                    sub_indices.append(i)

            if len(sub_indices) == 1:
                unfolded.append(action)
                continue
            last_src = src[sub_indices[-1]:]

            # dst transform
            dst = action[2]
            dst_tmp = []
            i = 0
            sub_indices = [0]
            for x in dst:
                i += 1
                if x == "array":
                    if dst[sub_indices[-1]] == "array":
                        dst_tmp.append(Action(action="coerce", dst=dst[sub_indices[-1]:], src=dst[i:]))
                    else:
                        dst_tmp.append(Action(action="coerce", dst=dst[sub_indices[-1]:], src=dst[i - 1:]))
                        dst_tmp.append(Action(action="coerce", dst=dst[i - 1:], src=dst[i:]))
                    sub_indices.append(i)

            # if len(sub_indices) == 1:
            #     raise ValueError("invalid operation {}".format(action))

            first_dst = dst[sub_indices[-1]:]
            unfolded.extend(src_tmp)
            unfolded.append(Action(action="coerce", src=last_src, dst=first_dst))
            unfolded.extend(reversed(dst_tmp))

        r = []
        for ac in unfolded:
            if ac[1] == ac[2]:
                continue
            r.append(ac)
        return r

    def simplify(self, unfolded_code):
        code = []
        for ac in unfolded_code:
            if ac[0] == "ref" and code and code[-1][0] == "deref":
                code.pop()
            elif ac[0] == "array" and code and code[-1][0] == "dearray":
                code.pop()
            else:
                code.append(ac)
        return code

    def post_gencode(self, unfolded_code):
        code = self.simplify(unfolded_code)

        stack = [[]]
        for ac in code:
            if ac[0] == "dearray":
                stack.append(["iterate"])
            elif ac[0] == "array":
                sub_code = stack.pop()
                stack[-1].append(tuple(sub_code))
            else:
                stack[-1].append(ac)
        assert len(stack) == 1
        return stack[0]


class MiniCodeGenerator(object):
    # generating mini language
    # [deref] -> *x
    # [ref] -> &x
    # [coerce, x, y] -> finding alias {original: x} and name is y, -> y(x)

    def __init__(self, resolver, normalizer=MiniCodeNormalizer()):
        self.resolver = resolver
        self.normalizer = normalizer

    def gencode(self, src_path, dst_path):
        mapping_path = self.resolver.resolve(src_path, dst_path)
        if mapping_path is None:
            msg = "mapping not found {!r} -> {!r}".format(src_path, dst_path)
            raise GencodeMappingNotFound(msg, src_path, dst_path)
        pre_gencode = self.normalizer.pre_gencode(mapping_path)
        code = self._gencode(pre_gencode)
        post_gencode = self.normalizer.post_gencode(code)
        return post_gencode

    def _gencode(self, mapping_path):
        code = []

        def get_primitive(v):
            if isinstance(v, (list, tuple)):
                return v[-1]
            else:
                return v

        for action in mapping_path:
            if get_primitive(action.src) == get_primitive(action.dst):
                if isinstance(action.src, (list, tuple)):
                    for typ in action.src[:-1]:
                        if typ == "pointer":
                            code.append(("deref", ))
                        elif typ == "array":
                            code.append(("dearray",))
                        else:
                            raise ValueError("not implemented: typ={}, path={}".format(typ, action.src))
                if isinstance(action.dst, (list, tuple)):
                    itr = reversed(action.dst)
                    next(itr)
                    for typ in itr:
                        if typ == "pointer":
                            code.append(("ref", ))
                        elif typ == "array":
                            code.append(("array",))
                        else:
                            raise ValueError("not implemented: typ={}, path={}".format(typ, action.dst))
            else:
                # todo: coerce
                code.append(Action(action=action[0], src=_unwrap_value(action[1]), dst=_unwrap_value(action[2])))
        return code


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--src", required=True)
    parser.add_argument("--dst", required=True)
    args = parser.parse_args()

    reader = Reader()

    with open(args.src) as rf:
        src_world = reader.read_world(json.load(rf, object_pairs_hook=OrderedDict))
        src_world.normalize()
    with open(args.dst) as rf:
        dst_world = reader.read_world(json.load(rf, object_pairs_hook=OrderedDict))
        dst_world.normalize()
    gencoder = MiniCodeGenerator(TypeMappingResolver([]))

    m = GoModule()
    m.package("convert")
    with m.import_group() as im:
        iw = ImportWriter(im)
        convertor = TypeConvertor(iw, gencoder, src_world, dst_world)
        iw.import_(src_world["model"])
        iw.import_(dst_world["def"])

    @convertor.as_override("string", src_world["bson"].fulladdress("ObjectId"))
    def string_to_object_id(convertor, m, value, *args):
        new_prefix = convertor.import_writer.import_(src_world["bson"])
        return "{}.ObjectIdHex({})".format(new_prefix, value)

    @convertor.as_override(src_world["bson"].fulladdress("ObjectId"), "string")
    def object_id_to_string(convertor, m, value, *args):
        return "{}.Hex()".format(value)

    @convertor.as_override(src_world["model"].fulladdress("Date"), "time.Time")
    def model_date_to_time(convertor, m, value, *args):
        return "{}.Time()".format(value)

    @convertor.as_override(src_world["bson"].fulladdress("ObjectId"), ("pointer", dst_world["def"].fulladdress("ID")))
    def object_id_to_id(convertor, m, value, *args):
        return "ConvertID({})".format(value)

    m.sep()
    cw = ConvertWriter(m, convertor)
    for module in dst_world.modules.values():
        for file in module.files.values():
            for struct in file.structs.values():
                for module in src_world.modules.values():
                    if struct.name in module:
                        print("@", struct.name, file=sys.stderr)
                        cw.write_function(module[struct.name], struct)
                    elif struct.name.startswith("Enduser") and struct.name[len("Enduser"):] in module:
                        print("<", struct.name, file=sys.stderr)
                        cw.write_function(module[struct.name[len("Enduser"):]], struct)
                    elif struct.name.startswith("Tuner") and struct.name[len("Tuner"):] in module:
                        print(">", struct.name, file=sys.stderr)
                        cw.write_function(module[struct.name[len("Tuner"):]], struct)

    # cw.write_function(src_world["model"]["Page"], dst_world["def"]["Page"])
    # # cw.write_function(dst_world["def"]["Page"], src_world["model"]["Page"])
    # cw.write_function(src_world["model"]["User"], dst_world["def"]["User"])

    print(m)


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    main()
