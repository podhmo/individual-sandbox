default:
	touch foo.txt
	touch bar.txt

00: foo.txt bar.txt
	echo $$< $<
	echo word $(word 2,$^)
	echo $$@ $@
	echo $$^ $^
