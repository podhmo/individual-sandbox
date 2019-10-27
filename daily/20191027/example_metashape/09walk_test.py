import unittest
from magicalimport import import_symbol

# from: glob2

glob = import_symbol("./08walk.py:glob")


class TestPatterns(unittest.TestCase):
    def test(self):
        d = {
            "dir1": {"a-file": "a1", "b-file": "b1"},
            "dir22": {"a-file": "a22", "b-file": "b22"},
        }
        got = glob(d, "dir?/a-*")
        want = [("dir1/a-file", "a1")]
        self.assertListEqual(list(got), list(want))


def makedirs(d, *paths):
    for path in paths:
        path = path.split("/")
        target = d
        for k in path:
            if k not in target:
                target[k] = {}
            target = target[k]
    return d


def touch(d, *paths, content=None):
    for path in paths:
        path = path.split("/")
        target = d
        for k in path[:-1]:
            target = target[k]
        target[path[-1]] = content
    return d


class TestRecursive(unittest.TestCase):
    d = {}
    makedirs(d, "a", "b", "a/foo")
    touch(
        d,
        "file.py",
        "file.txt",
        "a/bar.py",
        "README",
        "b/py",
        "b/bar.py",
        "a/foo/hello.py",
        "a/foo/world.txt",
    )

    def test_recursive(self):
        # ** includes the current directory
        got = sorted(glob(self.d, "**/*.py"))
        want = [
            ("a/bar.py", None),
            ("a/foo/hello.py", None),
            ("b/bar.py", None),
            ("file.py", None),
        ]
        self.assertListEqual(got, want)

    def test_exclude_root_directory(self):
        # If files from the root directory should not be included,
        # this is the syntax to use:
        got = sorted(glob(self.d, "*/**/*.py"))
        want = [("a/bar.py", None), ("a/foo/hello.py", None), ("b/bar.py", None)]
        self.assertListEqual(got, want)

    # def test_only_directories(self): # NG
    #     # Return directories only
    #     got = sorted(glob(self.d, "**/"))
    #     want = [("a/", ("a",)), ("a/foo/", ("a/foo",)), ("b/", ("b",))]
    #     self.assertListEqual(got, want)


#     def test_parent_dir(self):
#         # Make sure ".." can be used
#         os.chdir(path.join(self.basedir, "b"))
#         assert sorted(glob2.glob("../a/**/*.py", True)), [
#             ("../a/bar.py", ("", "bar")),
#             ("../a/foo/hello.py", ("foo", "hello")),
#         ]

#     def test_fixed_basename(self):
#         assert sorted(glob2.glob("**/bar.py", True)) == [
#             ("a/bar.py", ("a",)),
#             ("b/bar.py", ("b",)),
#         ]

#     def test_all_files(self):
#         # Return all files
#         os.chdir(path.join(self.basedir, "a"))
#         assert sorted(glob2.glob("**", True)) == [
#             ("bar.py", ("bar.py",)),
#             ("foo", ("foo",)),
#             ("foo/hello.py", ("foo/hello.py",)),
#             ("foo/world.txt", ("foo/world.txt",)),
#         ]

#     def test_root_directory_not_returned(self):
#         # Ensure that a certain codepath (when the basename is globbed
#         # with ** as opposed to the dirname) does not cause
#         # the root directory to be part of the result.
#         # -> b/ is NOT in the result!
#         assert sorted(glob2.glob("b/**", True)) == [
#             ("b/bar.py", ("bar.py",)),
#             ("b/py", ("py",)),
#         ]

#     def test_non_glob(self):
#         # Test without patterns.
#         assert glob2.glob(__file__, True) == [(__file__, ())]
#         assert glob2.glob(__file__) == [(__file__)]


# class TestIncludeHidden(BaseTest):
#     def setup_files(self):
#         self.makedirs("a", "b", "a/.foo")
#         self.touch(
#             "file.py",
#             "file.txt",
#             "a/.bar",
#             "README",
#             "b/py",
#             "b/.bar",
#             "a/.foo/hello.py",
#             "a/.foo/world.txt",
#         )

#     def test_hidden(self):
#         # ** includes the current directory
#         assert sorted(glob2.glob("*/*", True, include_hidden=True)), [
#             ("a/.bar", ("a", ".bar")),
#             ("a/.foo", ("a", ".foo")),
#             ("b/.bar", ("b", ".bar")),
#             ("b/py", ("b", "py")),
#         ]

if __name__ == "__main__":
    unittest.main()
