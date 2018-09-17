import os.path


def normalize_linesep_text(text):
    return os.linesep.join(line for line in text.splitlines())


print(repr(normalize_linesep_text("hello\n.bye bye.\n")))
print(repr(normalize_linesep_text("hello\r\n.bye bye.\r\n")))
print(repr(normalize_linesep_text("hello\r.bye bye.\r")))

# 'hello\n.bye bye.'
# 'hello\n.bye bye.'
# 'hello\n.bye bye.'
