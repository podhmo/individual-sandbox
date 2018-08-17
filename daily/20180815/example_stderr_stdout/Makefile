default: 00 01 02

00:
	python print.py | tee 00.output

# log file is stdout + stderr
01:
	# 2>&1 is also ok. if running not in Makefile
	python print.py 2>/dev/stdout | tee 01.output

# console output is stdout only, but log file is stdout + stderr
02:
	# swap is `3>&2 2>&1 1>&3`
	python print.py 1>tmp.pipe
#	python print.py tmp.pipe>/dev/stderr 2>/dev/stdout 1>tmp.pipe | tee 02.output

