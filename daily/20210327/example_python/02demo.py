from diff_match_patch import diff_match_patch

dmp = diff_match_patch()
diff = dmp.diff_main("Hello World.", "Goodbye World.")
# Result: [(-1, "Hell"), (1, "G"), (0, "o"), (1, "odbye"), (0, " World.")]
dmp.diff_cleanupSemantic(diff)
# Result: [(-1, "Hello"), (1, "Goodbye"), (0, " World.")]
print(diff)
print(type(diff))
