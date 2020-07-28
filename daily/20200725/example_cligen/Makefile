default: 00 01 02

SCRIPT := clsgen.py
SCRIPT := cligen.py

00:
	python $(SCRIPT) Person --name foo --age 20

# many classes
01:
	python $(SCRIPT) User --name foo - Team --name xxx --members '[{"name": "foo"}]'
# class with types
02:
	python $(SCRIPT) User --name --nickname "@typing.Optional[str]" xxx - Team --name xxx --members "@t.List[User]"
