from shape import shape


def run(d):
    print("")
    print(d)
    print(shape(d))


run({"name": "foo", "age": 10})
run({"person": {"name": "foo", "age": 10}})
run({"name": "foo", "age": 10, "skills": ["x", "y", "z"]})
run(
    {
        "name": "foo",
        "age": 10,
        "skills": [{
            "name": "x"
        }, {
            "name": "y"
        }, {
            "name": "z",
            "special": True
        }]
    }
)
run({"name": "foo", "age": 10, "skills": []})
run([{"name": "foo", "age": 10, "skills": []}, {"name": "foo", "age": 10}])
