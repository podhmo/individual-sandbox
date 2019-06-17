00:
	paste -d '@' a.txt b.txt | sed 's/\([^@]\)$$/\1\n/g; s/@$$//g' | tr @ '\n' | tee 00.out
01:
	paste -d '@' b.txt a.txt | sed 's/\([^@]\)$$/\1\n/g; s/@$$//g; s/^@//g' | grep -v -P '^$$'
