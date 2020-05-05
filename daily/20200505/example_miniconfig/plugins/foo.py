def includeme(c):
    def register():
        print("** foo")
    c.action(__name__, register)
