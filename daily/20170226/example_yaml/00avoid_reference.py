import yaml


class IgnoreReferenceDumper(yaml.Dumper):
    def ignore_aliases(self, data):
        return True


person = {
    "name": "foo",
    "age": 20
}
d = [person, person]

print(yaml.dump(d))
print(yaml.dump(d, Dumper=IgnoreReferenceDumper))
