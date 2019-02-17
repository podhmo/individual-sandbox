default: 00 01 02 03

00:
	kamidana 00error.j2 |& tee 00error.output
01:
	kamidana 01main.j2 |& tee 01main-error.output
02:
	kamidana 02main-use-macro.j2 |& tee 02use-macro-error.output
03:
	kamidana 03main-inheritance.j2 |& tee 03main-inheritance-error.output
clean:
	rm *.output
