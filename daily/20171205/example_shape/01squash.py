from shape import shape


def run(d, *, squash=False):
    print("")
    print(d)
    print(shape(d, squash=squash))


run({"name": "foo", "age": 10, "skills": []})
run([{"name": "foo", "age": 10, "skills": []}, {"name": "foo", "age": 10}])
run([{"name": "foo", "age": 10, "skills": []}, {"name": "foo", "age": 10}], squash=True)
