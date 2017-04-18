A = type("A", (), {"import": lambda self: "import!!"})
print(getattr(A(), "import")())  # import!!
# print(A().import) syntax error
