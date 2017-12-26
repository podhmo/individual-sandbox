from merge import merge

users = [
    {
        "id": 1,
        "name": "foo",
        "age": 20
    },
    {
        "id": 2,
        "name": "bar",
        "age": 10
    },
    {
        "id": 3,
        "name": "boo",
        "age": 10
    },
]

groups = [{
    "id": 1,
    "user_id": 1,
    "name": "A",
}, {
    "id": 2,
    "user_id": 3,
    "name": "B",
}]

print("inner")
print(merge(users, groups, left_on="id", right_on="user_id"))
print("outer")
print(merge(users, groups, how="outer", left_on="id", right_on="user_id"))
print("left")
print(merge(users, groups, how="left", left_on="id", right_on="user_id"))
print("right")
print(merge(users, groups, how="right", left_on="id", right_on="user_id"))
