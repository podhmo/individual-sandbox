from diff_match_patch import diff_match_patch

dmp = diff_match_patch()
before = """\
{
  "name": "foo",
  "age": 20,
  "type": "person"
}

"""
after = """\
{
  "name": "bar",
  "age": 21,
  "type": "person"
}
"""
diff = dmp.diff_lineMode(before, after, False)
dmp.diff_cleanupSemantic(diff)
print(diff)
print(type(diff))
