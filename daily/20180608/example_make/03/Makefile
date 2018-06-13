# 終了ステータスが1でも止まらない
run0:
	for i in 1 2 3; do python -c 'import sys; print(sys.argv); sys.exit(1 if int(sys.argv[1]) == 2 else 0)' $$i; done

# 終了ステータスが1なら止まる
run1: one two three

one:
	python -c 'import sys; print(sys.argv); sys.exit(0)' 1
two:
	python -c 'import sys; print(sys.argv); sys.exit(1)' 2
three:
	python -c 'import sys; print(sys.argv); sys.exit(0)' 3

