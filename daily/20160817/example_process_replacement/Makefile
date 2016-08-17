default:
	diff -u a.txt b.txt || echo ok
	echo "----------------------------------------"
	bash -c 'diff -u <(cat a.txt | sort) <(cat b.txt | sort) || echo ok'
