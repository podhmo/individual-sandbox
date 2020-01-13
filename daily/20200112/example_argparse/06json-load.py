from handofcats.actions._codeobject import codeobject, Module


@codeobject
def JSONDictType(m: Module, name: str) -> Module:
    toplevel = getattr(m, "toplevel", m)
    toplevel.import_("typing", as_="t")

    with m.def_("JSONDictType", "filename: str", return_type="t.Dict[str, t.Any]"):
        m.import_("json")
        with m.try_():
            with m.with_("open(filename)", as_="rf"):
                m.return_("json.load(rf)")
        with m.except_("Exception", "e"):
            m.stmt("raise argparse.ArgumentTypeError(str(e))")
    return m


m = Module()
m.stmt(JSONDictType)
print(m)
