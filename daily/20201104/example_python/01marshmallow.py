import u

s = u.UserSchema()
print(s.load([{"name": "foo"}, {"name": "bar"}], many=True))

ob = s.load({"name": "foo"})
print(ob)
