import sys
import argparse
import copy
import json
import logging
from collections import ChainMap, deque, defaultdict, namedtuple
from prestring.go import GoModule

Action = namedtuple("Action", "action, src, dst")
logger = logging.getLogger(__name__)


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
        return struct

    def read_alias(self, data, parent=None):
        alias = Alias(data["name"], data, parent=parent, reader=self)
        return alias


class GOWriter(object):
    prestring_module = GoModule

    def write_file(self, file, m=None):
        m = m or self.prestring_module()
        self.write_packagename(file, m=m)
        for struct in file.structs.values():
            self.write_struct(struct, m=m)
        for alias in file.aliases.values():
            self.write_alias(alias, m=m)
        return m

    def write_packagename(self, file, m=None):
        m = m or self.prestring_module()
        package_name = file.package_name
        if package_name is not None:
            m.package(package_name)
        return m

    def write_struct(self, struct, m=None):
        m = m or self.prestring_module()
        struct = struct.data
        self.write_comment(struct, m=m)
        with m.type_(struct["name"], "struct"):
            for field in sorted(struct["fields"].values(), key=lambda f: f["name"]):
                self.write_comment(field, m=m)
                if field["embed"]:
                    m.stmt(as_type(field["type"]))
                else:
                    m.stmt("{} {}".format(field["name"], as_type(field["type"])))
                if "tags" in field:
                    m.insert_after("  ")
                    for tag in field["tags"]:
                        m.insert_after(tag)
        return m

    def write_alias(self, alias, m=None):
        m = m or self.prestring_module()
        alias = alias.data
        m.type_alias(alias["name"], alias["original"]["value"])
        with m.const_group() as const:
            for c in alias.get("candidates", []):
                self.write_comment(c, m=const) or const.comment("{} : a member of {}".format(c["name"], alias["name"]))
                const("{} {} = {}".format(c["name"], alias["name"], c["value"]))
        return m

    def write_comment(self, target, m=None):
        m = m or self.prestring_module()
        if "comment" in target:
            m.comment(target["comment"])
            return m
        else:
            return None


def as_type(type_dict):
    kind = type_dict.get("kind", "primitive")
    if kind == "primitive":
        return type_dict["value"]
    elif kind == "pointer":
        return "*{}".format(as_type(type_dict["value"]))
    elif kind == "selector":
        return "{}".format(as_type(type_dict["value"]))
    elif kind == "array":
        return "[]{}".format(as_type(type_dict["value"]))
    else:
        raise ValueError("unknown type: {}".format(type_dict))


class World(object):
    def __init__(self, parent=None, reader=None):
        self.parent = parent
        self.reader = reader
        self.modules = {}
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
        self.files = {}
        self.reset()

    @property
    def package_name(self):
        return self.name

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


class File(object):
    def __init__(self, name, imports, parent=None, reader=None):
        self.name = name
        self.imports = imports
        self.parent = parent
        self.reader = reader
        self.aliases = {}
        self.structs = {}
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

    def dump(self, writer):
        return writer.write_alias(self)

    def normalize(self):
        pass


class Struct(object):
    def __init__(self, name, data, parent=None, reader=None):
        self.name = name
        self.parent = parent
        self.reader = reader
        self.rawdata = data
        self.data = data

    @property
    def package_name(self):
        if self.parent is None:
            return None
        return self.parent.package_name

    def dump(self, writer):
        return writer.write_struct(self)

    def __getitem__(self, name):
        return self.data["fields"][name]

    def __contains__(self, name):
        return name in self.data["fields"]

    def fields(self):
        return self.data["fields"].items()

    def normalize(self):
        self.rawdata = copy.deepcopy(self.rawdata)
        data = self.data["fields"]
        for k in data.keys():
            data[k.lower()] = data.pop(k)


def write_covert_function(m, convertor, src, dst):
    src_arg = 'src *{}.{}'.format(src.package_name, src.name)
    dst_arg = '*{}.{}'.format(dst.package_name, dst.name)
    with m.func('ConvertFrom{}{}'.format(src.package_name.title(), dst.name), src_arg, return_="({}, error)".format(dst_arg)):
        m.stmt("dst := &{}.{}{{}}".format(dst.package_name, dst.name))
        for name, field in sorted(dst.fields()):
            if name in src:
                m.stmt("dst.{} = {}".format(field["name"], convertor.convert(m, src[name], field, src, dst, name="src.{}".format(src[name]["name"]))))  # xxx
            else:
                m.comment("FIXME: {} is not found".format(field["name"]))
        m.return_("dst, nil")
    return m


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


class TypeConvertor(object):
    def __init__(self, import_writer, codegen, src_world, dst_world):
        self.import_writer = import_writer
        self.codegen = codegen
        self.src_world = src_world
        self.dst_world = dst_world
        self.i = 0
        items = []
        for _, module in self.src_world.modules.items():
            for _, file in module.files.items():
                for _, alias in file.aliases.items():
                    # todo: recursive
                    data = alias.data
                    items.append((data["original"]["value"], "{}.{}".format(file.parent.fullname, data["name"])))
        for _, module in self.dst_world.modules.items():
            for _, file in module.files.items():
                for _, alias in file.aliases.items():
                    # todo: recursive
                    data = alias.data
                    items.append((data["original"]["value"], "{}.{}".format(file.parent.fullname, data["name"])))
        self.codegen.resolver.add(items)

    def get_type_path(self, value, struct):
        if value["kind"] == "primitive":
            module = struct.parent.parent
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

    def get_new_prefix_on_selector(self, prefix, dst_struct):
        dst_module = dst_struct.parent.parent
        if dst_module.name == prefix:
            fullname = dst_module.fullname
        else:
            fullname = dst_struct.parent.imports[prefix]["fullname"]
        module = dst_module.parent.modules_by_fullname[fullname]
        # writing import clause if needed
        return self.import_writer.import_(module)

    def coerce(self, src_type, dst_type, src_struct, dst_struct, value):
        if isinstance(dst_type, (list, tuple)):
            "({})({})".format("".join("*" if x == "pointer" else x for x in dst_type), value)
        elif src_type == "string" and dst_type == "gopkg.in/mgo.v2/bson.ObjectId":
            module = dst_struct.parent.parent.parent.modules_by_fullname["gopkg.in/mgo.v2/bson"]
            new_prefix = self.import_writer.import_(module)
            return "{}.ObjectIdHex({})".format(new_prefix, value)
        elif src_type == "gopkg.in/mgo.v2/bson.ObjectId" and dst_type == "string":
            return "{}.Hex()".format(value)
        elif "/" in dst_type:
            prefix_and_name = dst_type.rsplit("/", 1)[-1]
            prefix, name = prefix_and_name.rsplit(".")
            new_prefix = self.get_new_prefix_on_selector(prefix, dst_struct)
            return "{}.{}({})".format(new_prefix, name, value)
        else:
            return "{}({})".format(dst_type, value)

    def convert(self, m, src, dst, src_struct, dst_struct, name):
        src_path = list(self.get_type_path(src["type"], src_struct))
        dst_path = list(self.get_type_path(dst["type"], dst_struct))
        code = self.codegen.gencode(src_path, dst_path)
        value = name
        is_cast = False
        for op in code:
            if is_cast:
                tmp = "tmp{}".format(self.i)
                self.i += 1
                m.stmt("{} := {}".format(tmp, value))
                value = tmp
                is_cast = False

            if op[0] == "deref":
                value = "*({})".format(value)
            elif op[0] == "ref":
                value = "&({})".format(value)
                is_cast = True
            elif op[0] == "coerce":
                value = self.coerce(op[1], op[2], src_struct, dst_struct, value)
                is_cast = True
        return value


class TypeMappingResolver(object):
    def __init__(self, items):
        self.primitive_map = defaultdict(list)
        self.detail_map = defaultdict(list)
        self.add(items)

    def add(self, items):
        for k, v in items:
            k = self._wrap_value(k)
            v = self._wrap_value(v)
            self._add_value(k, v)
            self._add_value(v, k)

    def _has_detail(self, wv):
        return len(wv) > 1

    def _add_value(self, k, v):
        self.detail_map[k].append(v)
        if self._has_detail(k):
            self.primitive_map[k[-1:]].append((k, v))
            self.primitive_map[v[-1:]].append((v, k))

    def _wrap_value(self, v):
        if isinstance(v, (tuple, list)):
            return tuple(v)
        else:
            return (v,)

    def _unwrap_value(self, v):
        if not self._has_detail(v):
            return v[0]
        else:
            return v

    def resolve(self, src, dst):
        if src == dst:
            return []
        src = self._wrap_value(src)
        dst = self._wrap_value(dst)
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
            prev, next_ = self._unwrap_value(path[i]), self._unwrap_value(path[i + 1])
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


class MiniCodeGenerator(object):
    # generating mini language
    # [deref] -> *x
    # [ref] -> &x
    # [coerce, x, y] -> finding alias {original: x} and name is y, -> y(x)

    def __init__(self, resolver, optimizer=None):
        self.resolver = resolver
        self.optimizer = optimizer or self._default_optimize

    def _default_optimize(self, code):
        optimized = []
        for x in code:
            if x[0] == "ref" and optimized and optimized[-1][0] == "deref":
                optimized.pop()
            else:
                optimized.append(x)
        return optimized

    def gencode(self, src_path, dst_path):
        code = self._gencode(src_path, dst_path)
        code = self.optimizer(code)
        return code

    def _gencode(self, src_path, dst_path):
        code = []
        mapping_path = self.resolver.resolve(src_path, dst_path)
        if mapping_path is None:
            raise ValueError("mapping not found {!r} -> {!r}".format(src_path, dst_path))

        def get_primitive(v):
            if isinstance(v, (list, tuple)):
                return v[-1]
            else:
                return v

        for action in mapping_path:
            if get_primitive(action.src) == get_primitive(action.dst):
                if isinstance(action.src, (list, tuple)):
                    itr = reversed(action.src)
                    next(itr)
                    for typ in itr:
                        if typ == "pointer":
                            code.append(("deref", ))
                        else:
                            raise ValueError("not implemented: typ={}, path={}".format(typ, src_path[1:]))
                if isinstance(action.dst, (list, tuple)):
                    itr = reversed(action.dst)
                    next(itr)
                    for typ in itr:
                        if typ == "pointer":
                            code.append(("ref", ))
                        else:
                            raise ValueError("not implemented: typ={}, path={}".format(typ, src_path[1:]))
            else:
                code.append(action)
        return code


def detect_pointer_level(path):
    return len(path) - 1  # xxx


def sandbox(writer, reader, src_world, dst_world):
    print(src_world["model"]["Page"])
    print(dst_world["def"]["Page"])
    print(src_world["model"]["Page"].package_name)
    print(dst_world["def"]["Page"].package_name)
    print(src_world["model"]["Page"].dump(writer))
    print(dst_world["def"]["Page"].dump(writer))
    print("----------------------------------------")
    # print("----------------------------------------")
    # print(gencoder.gencode(src_path=["string"], dst_path=["string"]))
    # print(gencoder.gencode(src_path=["string", "pointer"], dst_path=["string"]))
    # print(gencoder.gencode(src_path=["string"], dst_path=["string", "pointer"]))
    # print(gencoder.gencode(src_path=["string", "pointer"], dst_path=["string", "pointer"]))
    # print(gencoder.gencode(src_path=["string", "pointer", "pointer"], dst_path=["string", "pointer"]))
    # print(gencoder.gencode(src_path=["string", "pointer", "pointer"], dst_path=["string"]))
    # print(gencoder.gencode(src_path=["string", "pointer"], dst_path=["X", "pointer"]))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--src", required=True)
    parser.add_argument("--dst", required=True)
    args = parser.parse_args()

    writer = GOWriter()
    reader = Reader()

    with open(args.src) as rf:
        src_world = reader.read_world(json.load(rf))
        src_world.normalize()
    with open(args.dst) as rf:
        dst_world = reader.read_world(json.load(rf))
        dst_world.normalize()
    # sandbox(writer, reader, src_world, dst_world)
    gencoder = MiniCodeGenerator(TypeMappingResolver([]))

    m = GoModule()
    m.package("convert")
    with m.import_group() as im:
        import_writer = ImportWriter(im)
        convertor = TypeConvertor(import_writer, gencoder, src_world, dst_world)
        import_writer.import_(src_world["model"])
        import_writer.import_(dst_world["def"])
    m.sep()
    write_covert_function(m, convertor, src_world["model"]["Page"], dst_world["def"]["Page"])
    write_covert_function(m, convertor, dst_world["def"]["Page"], src_world["model"]["Page"])
    print(m)

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    main()
