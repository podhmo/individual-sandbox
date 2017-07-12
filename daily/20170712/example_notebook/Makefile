OPTS = --ExecutePreprocessor.timeout=600 
TARGET ?= Untitled.ipynb
# Options include ['asciidoc', 'custom', 'html', 'latex', 'markdown', 'notebook', 'pdf', 'python', 'rst', 'script', 'slides']

html:
	jupyter nbconvert ${OPTS} --to html ${TARGET}
# pdf:
# 	jupyter nbconvert ${OPTS} --to pdf ${TARGET}
markdown:
	jupyter nbconvert ${OPTS} --to markdown ${TARGET}
script:
	jupyter nbconvert ${OPTS} --to script ${TARGET}
notebook:
	jupyter nbconvert --inplace ${OPTS} --execute --to notebook ${TARGET}

00:
	jupyter nbconvert ${OPTS} --execute --to notebook 00random.ipynb --output 00output
01:
	jupyter nbconvert ${OPTS} --execute --to notebook 01graph.ipynb
