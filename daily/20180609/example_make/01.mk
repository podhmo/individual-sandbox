.%.title: %.md
	cat $< | grep -P '^#[^#]' | tee $@

default: docs/a/.readme.title docs/b/.readme.title docs/b/x/.readme.title docs/b/y/.readme.title docs/c/.readme.title
	cat $^

docs/a/.readme.title: docs/a/readme.md
docs/b/.readme.title: docs/b/readme.md
docs/b/.x/readme.title: docs/b/x/readme.md
docs/b/.y/readme.title: docs/b/x/readme.md
docs/c/.readme.title: docs/c/readme.md

clean:
	find . -name ".readme.title" | xargs rm -vf
.PHONY: clean
