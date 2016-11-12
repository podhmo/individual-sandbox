# -*- coding:utf-8 -*-
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
        return "{}".format(as_type(type_dict["value"]))  # xxxx: bug
    elif kind == "array":
        return "[]{}".format(as_type(type_dict["value"]))
    else:
        raise ValueError("unknown type: {}".format(type_dict))
