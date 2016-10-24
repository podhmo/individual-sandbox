import argparse
import copy
import json
import logging
from collections import ChainMap
from prestring.go import GoModule


logger = logging.getLogger(__name__)


class Reader(object):
    def read_world(self, data, parent=None):
        world = World(parent=parent, reader=self)
        for name, module in data["module"].items():
            world.read_module(name, module)
        return world

    def read_module(self, data, parent=None):
        module = Module(data["name"], parent=parent, reader=self)
        for name, file in data["file"].items():
            module.read_file(name, file)
        return module

    def read_file(self, data, parent=None):
        file = File(data["name"], parent=parent, reader=self)
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

    def read_module(self, name, module):
        self.modules[name] = self.reader.read_module(module, parent=self)

    def normalize(self):
        for module in self.modules.values():
            module.normalize()

    def __getitem__(self, name):
        return self.modules[name]


class Module(object):
    def __init__(self, name, parent=None, reader=None):
        self.name = name
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
    def __init__(self, name, parent=None, reader=None):
        self.name = name
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


def write_covert_function(convertor, src, dst, writer):
    m = writer.prestring_module()
    src_arg = 'src *{}.{}'.format(src.package_name, src.name)
    dst_arg = '*{}.{}'.format(dst.package_name, dst.name)
    with m.func('ConvertFrom{}'.format(dst.name), src_arg, return_="({}, error)".format(dst_arg)):
        m.stmt("dst := &{}.{}{{}}".format(dst.package_name, dst.name))
        for name, field in dst.fields():
            if name in src:
                m.stmt("dst.{} = {}".format(field["name"], convertor.convert(src[name], field, name="src.{}".format(src[name]["name"]))))  # xxx
            else:
                m.comment("FIXME: {} is not found".format(name))
        m.return_("dst, nil")
    return m


class TypeConvertor(object):
    def __init__(self, resolver, src_world, dst_world):
        self.resolver = resolver
        self.src_world = src_world
        self.dst_world = dst_world

    def get_type_path(self, value):
        if value["kind"] == "primitive":
            return [value["value"]]
        elif value["kind"] == "selector":
            return [value["value"]]
        else:
            r = [value["kind"]]
            r.extend(self.get_type_path(value["value"]))
            return r

    def get_coerce(self, src_type, dst_type, src, dst):
        # primitive -> y
        # x -> primitive
        # x -> y => x -> primitive -> y
        # TODO: implementation
        return dst_type

    def convert(self, src, dst, name):
        src_path = list(reversed(self.get_type_path(src["type"])))
        dst_path = list(reversed(self.get_type_path(dst["type"])))
        code = self.resolver.resolve(src_path, dst_path)
        value = name
        for op in code:
            if op[0] == "deref":
                value = "*({})".format(value)
            elif op[0] == "ref":
                value = "&({})".format(value)
            elif op[0] == "coerce":
                value = "{}({})".format(self.get_coerce(op[1], op[2], src, dst), value)
        return value


class TypeConverCodeResolver(object):
    # generating mini language
    # [deref] -> *x
    # [ref] -> &x
    # [coerce, x, y] -> finding alias {original: x} and name is y, -> y(x)

    def __init__(self, optimizer=None):
        self.optimizer = optimizer or self._default_optimize

    def _default_optimize(self, code):
        optimized = []
        for x in code:
            if x[0] == "ref" and optimized and optimized[-1][0] == "deref":
                optimized.pop()
            else:
                optimized.append(x)
        return optimized

    def resolve(self, src_path, dst_path):
        code = self._resolve(src_path, dst_path)
        code = self.optimizer(code)
        return code

    def _resolve(self, src_path, dst_path):
        code = []
        if src_path[0] != dst_path[0]:
            code.append(["coerce", src_path[0], dst_path[0]])
        for typ in src_path[1:]:
            if typ == "pointer":
                code.append(["deref"])
            else:
                raise ValueError("not implemented: typ={}, path={}".format(typ, src_path[1:]))
        for typ in dst_path[1:]:
            if typ == "pointer":
                code.append(["ref"])
            else:
                raise ValueError("not implemented: typ={}, path={}".format(typ, src_path[1:]))
        return code


def sandbox(writer, reader, src_world, dst_world):
    print(src_world["model"]["Page"])
    print(dst_world["def"]["Page"])
    print(src_world["model"]["Page"].package_name)
    print(dst_world["def"]["Page"].package_name)
    print(src_world["model"]["Page"].dump(writer))
    print(dst_world["def"]["Page"].dump(writer))
    print("----------------------------------------")
    # print("----------------------------------------")
    # print(resolver.resolve(src_path=["string"], dst_path=["string"]))
    # print(resolver.resolve(src_path=["string", "pointer"], dst_path=["string"]))
    # print(resolver.resolve(src_path=["string"], dst_path=["string", "pointer"]))
    # print(resolver.resolve(src_path=["string", "pointer"], dst_path=["string", "pointer"]))
    # print(resolver.resolve(src_path=["string", "pointer", "pointer"], dst_path=["string", "pointer"]))
    # print(resolver.resolve(src_path=["string", "pointer", "pointer"], dst_path=["string"]))
    # print(resolver.resolve(src_path=["string", "pointer"], dst_path=["X", "pointer"]))


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
    resolver = TypeConverCodeResolver()
    convertor = TypeConvertor(resolver, src_world, dst_world)

    m = GoModule()
    m.package("convert")
    with m.import_group() as im:
        im.import_("github.com/podhmo/hmm/{}".format(src_world["model"].package_name))
        im.import_("github.com/podhmo/hmm/{}".format(dst_world["def"].package_name))
    m.sep()
    print(m)
    print(write_covert_function(convertor, src_world["model"]["Page"], dst_world["def"]["Page"], writer))


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    main()
