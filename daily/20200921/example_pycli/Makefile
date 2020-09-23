00:
	python -m rich
01:
	python -m rich.control
02:
	python -m rich.tabulate
03:
	python -m rich.styled
04:
	python -m rich.emoji
05:
	python -m rich.theme
06:
	python -m rich.cells
07:
	python -m rich.highlighter
08:
	python -m rich.rule
09:
	python -m rich.align
10:
	python -m rich.logging
11:
	python -m rich.markup
12:
	python -m rich.bar
13:
	python -m rich.box
14:
	python -m rich.columns
15:
	python -m rich.panel
16:
	python -m rich.segment
17:
	python -m rich.prompt
18:
	python -m rich.pretty
19:
	python -m rich.markdown readme.md
20:
	python -m rich.color
21:
	python -m rich.syntax $(shell python -c 'import rich.syntax as m; print(m.__file__)')
22:
	python -m rich.table
23:
	python -m rich.console
24:
	python -m rich.progress
25:
	python -m rich.text

# bullet
50:
	python $(shell echo $@*.py)
51:
	python $(shell echo $@*.py)
52:
	python $(shell echo $@*.py)
53:
	python $(shell echo $@*.py)


# clikan
export CLIKAN_HOME=.
60:
	clikan configure
61:
	clikan add foo
	clikan add bar
	clikan add boo
62:
	clikan regress 2
	clikan promote 3
63:
	clikan show

# npm install -g diff2html-cli
70:
	diff -u 50*.py 51*.py > a.diff || :
	diff2html -i file -- a.diff > a.html
	diff2html -i file -s side -- a.diff > b.html

clean:
	rm -f a.diff
.PHONY: clean
