class T:
    def __init__(self, restriction):
        self.restriction = restriction

    def __str__(self):
        return "t"

    def __getattr__(self, name):
        if name not in self.restriction:
            raise RuntimeError(f"unexpected field found: {name!r}")
        return name


def fetchall(db, tmpl, *args):
    print(tmpl)


def main():
    db = object()
    t = T({"name": str, "age": int})

    fetchall(db, f"""SELECT {t.name}, {t.age} from {t}""")

    # error
    # fetchall(db, f"""SELECT {t.name}, {t.age}, {t.missing} from {t}""")


if __name__ == "__main__":
    main()
