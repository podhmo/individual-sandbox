class Shaper:
    def __init__(self, iterate=sorted):
        self.iterate = iterate

    def shape(self, d):
        r = set()
        self._shape(d, r, [])
        return r

    def _shape(self, d, r, path, strict=True):
        if hasattr(d, "keys"):
            self._shape_dict(d, r, path)
        elif isinstance(d, (list, tuple)):
            self._shape_list(d, r, path)
        else:
            self._shape_atom(d, r, path, strict=strict)

    def _shape_dict(self, d, r, path):
        for k in self.iterate(d.keys()):
            path.append(k)
            self._shape(d[k], r, path, strict=True)
            path.pop()

    def _shape_list(self, xs, r, path):
        path.append("[]")
        for x in self.iterate(xs):
            self._shape(x, r, path, strict=False)
        path.pop()

    def _shape_atom(self, v, r, path, strict):
        # TODO: support strict
        shape = path[:]
        shape.append(v)
        r.add(tuple(shape))


def main():
    person = {
        "type": "object",
        "properties": {
            "name": {
                "type": "string"
            },
            "age": {
                "type": "integer"
            }
        }
    }
    person2 = {
        "type": "object",
        "properties": {
            "name": {
                "type": "string"
            },
            "age": {
                "type": "integer",
                "minimum": 0
            }
        }
    }

    shaper = Shaper(sorted)
    print(shaper.shape(person))
    print(shaper.shape(person2))

if __name__ == "__main__":
    main()
