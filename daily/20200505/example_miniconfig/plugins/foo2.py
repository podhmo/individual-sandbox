def includeme(c):
    def register():
        print("** foo2")

    c.action("plugins.foo", register)  # conflict foo.py
