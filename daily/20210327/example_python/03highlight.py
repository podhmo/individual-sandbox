from diff_match_patch import diff_match_patch


def color_diff(diff):
    state = None
    before = []
    after = []

    def flush():
        nonlocal before, after
        assert len(before) == len(after)

        buf = []
        for i in range(len(before)):
            dmp = diff_match_patch()
            diff = dmp.diff_main(before[i][1:], after[i][1:])
            dmp.diff_cleanupSemantic(diff)
            line = ["\x1b[1;31m-"]
            buf.append(["\x1b[1;32m+"])
            for i, text in diff:
                if i == diff_match_patch.DIFF_EQUAL:
                    line.append(text)
                    buf[-1].append(text)
                elif i == diff_match_patch.DIFF_INSERT:
                    buf[-1].append("\x1b[7m")
                    buf[-1].append(text)
                    buf[-1].append("\x1b[27m")
                elif i == diff_match_patch.DIFF_DELETE:
                    line.append("\x1b[7m")
                    line.append(text)
                    line.append("\x1b[27m")
                else:
                    raise ValueError(f"unexpected type {i}")
            line.append("\x1b[0m")
            yield "".join(line)

        for line in buf:
            line.append("\x1b[0m")
            yield "".join(line)

        before = []
        after = []

    for line in diff:
        if line.startswith("+ "):
            state = "+"
            after.append(line)
        elif line.startswith("- "):
            state = "-"
            before.append(line)
        elif line.startswith("@"):
            if state != "@" and before:
                yield from flush()
            state = "@"
            yield f"\x1b[36m{line}\x1b[0m"
        else:
            if state != " " and before:
                yield from flush()
            state = " "
            yield line

    if before:
        yield from flush()


s = """\
--- src/000person.json	2021-03-27 13:02:45.000000000 +0900
+++ src/001person.json	2020-01-08 06:16:00.000000000 +0900
@@ -1,5 +1,5 @@
 {
-  "name": "foo",
-  "age": 20,
+  "name": "bar",
+  "age": 21,
   "type": "person"
 }
"""
for line in color_diff(s.split("\n")):
    print(line)
