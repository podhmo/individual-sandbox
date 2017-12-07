import parselib


def run(filepath):
    t = parselib.parse_file(filepath)
    parselib.dump_tree(t, tostring=parselib.node_onlydef_tostring)


# run("./src/00.py")
# run("./src/01.py")
run("./src/03.py")
